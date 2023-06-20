unit u_mp3gain;

{$mode ObjFPC}{$H+}
// disable compiler warning "Comment level 2 found" because there is C code
// inside pascal comment, and C use '{' as block begin.
 {$WARN 2005 off : Comment level $1 found}
interface

uses
  Classes, SysUtils, ctypes;


{*
 *  ReplayGainAnalysis - analyzes input samples and give the recommended dB change
 *  Copyright (C) 2001-2009 David Robinson and Glen Sawyer
 *  Improvements and optimizations added by Frank Klemm, and by Marcel Müller
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  concept and filter values by David Robinson (David@Robinson.org)
 *    -- blame him if you think the idea is flawed
 *  original coding by Glen Sawyer (mp3gain@hotmail.com)
 *    -- blame him if you think this runs too slowly, or the coding is otherwise flawed
 *
 *  lots of code improvements by Frank Klemm ( http://www.uni-jena.de/~pfk/mpp/ )
 *    -- credit him for all the _good_ programming ;)
 *
 *
 *  For an explanation of the concepts and the basic algorithms involved, go to:
 *    http://www.replaygain.org/
 *}

{*
 *  Here's the deal. Call
 *
 *    InitGainAnalysis ( long samplefreq );
 *
 *  to initialize everything. Call
 *
 *    AnalyzeSamples ( const Float_t*  left_samples,
 *                     const Float_t*  right_samples,
 *                     size_t          num_samples,
 *                     int             num_channels );
 *
 *  as many times as you want, with as many or as few samples as you want.
 *  If mono, pass the sample buffer in through left_samples, leave
 *  right_samples NULL, and make sure num_channels = 1.
 *
 *    GetTitleGain()
 *
 *  will return the recommended dB level change for all samples analyzed
 *  SINCE THE LAST TIME you called GetTitleGain() OR InitGainAnalysis().
 *
 *    GetAlbumGain()
 *
 *  will return the recommended dB level change for all samples analyzed
 *  since InitGainAnalysis() was called and finalized with GetTitleGain().
 *
 *  Pseudo-code to process an album:
 *
 *    Float_t       l_samples [4096];
 *    Float_t       r_samples [4096];
 *    size_t        num_samples;
 *    unsigned int  num_songs;
 *    unsigned int  i;
 *
 *    InitGainAnalysis ( 44100 );
 *    for ( i = 1; i <= num_songs; i++ ) {
 *        while ( ( num_samples = getSongSamples ( song[i], left_samples, right_samples ) ) > 0 )
 *            AnalyzeSamples ( left_samples, right_samples, num_samples, 2 );
 *        fprintf ("Recommended dB change for song %2d: %+6.2f dB\n", i, GetTitleGain() );
 *    }
 *    fprintf ("Recommended dB change for whole album: %+6.2f dB\n", GetAlbumGain() );
 *}

{*
 *  So here's the main source of potential code confusion:
 *
 *  The filters applied to the incoming samples are IIR filters,
 *  meaning they rely on up to <filter order> number of previous samples
 *  AND up to <filter order> number of previous filtered samples.
 *
 *  I set up the AnalyzeSamples routine to minimize memory usage and interface
 *  complexity. The speed isn't compromised too much (I don't think), but the
 *  internal complexity is higher than it should be for such a relatively
 *  simple routine.
 *
 *  Optimization/clarity suggestions are welcome.
 *}


{
  This unit is a pascal conversion of "gain_analysis.c"
  at "https://github.com/Sound-Linux-More/mp3gain/blob/master/src/gain_analysis.c"
  converted by lulu
}

const
  GAIN_NOT_ENOUGH_SAMPLES = -24601;


type
  Float_t = double;         // Type used for filtering
  PFloat_t = ^Float_t;

 function InitGainAnalysis(samplefreq: clong ): boolean;
 function AnalyzeSamples(left_samples,                // samples are 16bit integer in type double
                         right_samples: PFloat_t;     // range is [-32767..32768]
                         num_samples: csize_t;
                         num_channels: cint): boolean;
 function ResetSampleFrequency(samplefreq: clong): boolean;
 function GetTitleGain: Float_t;
 function GetAlbumGain: Float_t;


implementation
uses Math;

type
  Uint32_t = cuint;
  PUint32_t = ^Uint32_t;
  Int32_t = cint;

const
  YULE_ORDER  = 10;
  BUTTER_ORDER = 2;
  RMS_PERCENTILE = 0.95;    // percentile which is louder than the proposed level
  MAX_SAMP_FREQ_KHZ = 96;   // maximum allowed sample frequency [kHz]
  RMS_WINDOW_TIME_MS = 50;  // Time slice size [ms]
  STEPS_per_dB = 100;       // Table entries per dB
  MAX_dB = 120;             // Table entries for 0...MAX_dB (normal max. values are 70...80 dB)

  PINK_REF = 64.82; // calibration value

  MAX_ORDER = 10;

  MAX_SAMPLES_PER_WINDOW = MAX_SAMP_FREQ_KHZ * RMS_WINDOW_TIME_MS + 1; // max. Samples per Time slice

var

  linprebuf: array[0..MAX_ORDER * 2-1] of Float_t;
  linpre: PFloat_t;     // left input samples, with pre-buffer
  lstepbuf: array[0..MAX_SAMPLES_PER_WINDOW + MAX_ORDER-1] of Float_t;
  lstep: PFloat_t;      // left "first step" (i.e. post first filter) samples
  loutbuf: array[0..MAX_SAMPLES_PER_WINDOW + MAX_ORDER-1] of Float_t;
  lout: PFloat_t;       // left "out" (i.e. post second filter) samples
  rinprebuf: array[0..MAX_ORDER * 2-1] of Float_t;
  rinpre: PFloat_t;     // right input samples ...
  rstepbuf: array[0..MAX_SAMPLES_PER_WINDOW + MAX_ORDER-1] of Float_t;
  rstep: PFloat_t;
  routbuf: array[0..MAX_SAMPLES_PER_WINDOW + MAX_ORDER-1] of Float_t;
  rout: PFloat_t;
  sampleWindow: clong;  // number of samples required to reach number of milliseconds required for RMS window
  totsamp: clong;
  lsum,
  rsum: double;
  freqindex: cint;

  A: array[0..(STEPS_per_dB * MAX_dB)-1] of Uint32_t;
  B: array[0..(STEPS_per_dB * MAX_dB)-1] of Uint32_t;

// for each filter:
// [0] 48 kHz, [1] 44.1 kHz, [2] 32 kHz, [3] 24 kHz, [4] 22050 Hz, [5] 16 kHz, [6] 12 kHz, [7] is 11025 Hz, [8] 8 kHz

const

  ABYule: array[0..11, 0..2*YULE_ORDER+1-1] of Float_t = (
    (0.006471345933032, -7.22103125152679, -0.02567678242161,  24.7034187975904,   0.049805860704367, -52.6825833623896,  -0.05823001743528,  77.4825736677539,   0.040611847441914, -82.0074753444205,  -0.010912036887501, 63.1566097101925,  -0.00901635868667,  -34.889569769245,    0.012448886238123, 13.2126852760198,  -0.007206683749426, -3.09445623301669,  0.002167156433951, 0.340344741393305, -0.000261819276949),
    (0.015415414474287, -7.19001570087017, -0.07691359399407,  24.4109412087159,   0.196677418516518, -51.6306373580801,  -0.338855114128061, 75.3978476863163,   0.430094579594561, -79.4164552507386,  -0.415015413747894, 61.0373661948115,   0.304942508151101, -33.7446462547014,  -0.166191795926663, 12.8168791146274,   0.063198189938739, -3.01332198541437, -0.015003978694525, 0.223619893831468,  0.001748085184539),
    (0.021776466467053, -5.74819833657784, -0.062376961003801, 16.246507961894,    0.107731165328514, -29.9691822642542,  -0.150994515142316, 40.027597579378,    0.170334807313632, -40.3209196052655,  -0.157984942890531, 30.8542077487718,   0.121639833268721, -17.5965138737281,  -0.074094040816409,  7.10690214103873,  0.031282852041061, -1.82175564515191, -0.00755421235941,  0.223619893831468,  0.00117925454213 ),
    (0.03857599435200,  -3.84664617118067, -0.02160367184185,   7.81501653005538, -0.00123395316851,  -11.34170355132042, -0.00009291677959,  13.05504219327545, -0.01655260341619,  -12.28759895145294,  0.02161526843274,   9.48293806319790, -0.02074045215285,   -5.87257861775999,  0.00594298065125,   2.75465861874613,  0.00306428023191,  -0.86984376593551,  0.00012025322027,  0.13919314567432,   0.00288463683916 ),
    (0.05418656406430,  -3.47845948550071, -0.02911007808948,   6.36317777566148, -0.00848709379851,   -8.54751527471874, -0.00851165645469,   9.47693607801280, -0.00834990904936,   -8.81498681370155,  0.02245293253339,   6.85401540936998, -0.02596338512915,   -4.39470996079559,  0.01624864962975,   2.19611684890774, -0.00240879051584,  -0.75104302451432,  0.00674613682247,  0.13149317958808,  -0.00187763777362 ),
    (0.15457299681924,  -2.37898834973084, -0.09331049056315,   2.84868151156327, -0.06247880153653,   -2.64577170229825,  0.02163541888798,   2.23697657451713, -0.05588393329856,   -1.67148153367602,  0.04781476674921,   1.00595954808547,  0.00222312597743,   -0.45953458054983,  0.03174092540049,   0.16378164858596, -0.01390589421898,  -0.05032077717131,  0.00651420667831,  0.02347897407020,  -0.00881362733839 ),
    (0.30296907319327,  -1.61273165137247, -0.22613988682123,   1.07977492259970, -0.08587323730772,   -0.25656257754070,  0.03282930172664,  -0.16276719120440, -0.00915702933434,   -0.22638893773906, -0.02364141202522,   0.39120800788284, -0.00584456039913,   -0.22138138954925,  0.06276101321749,   0.04500235387352, -0.00000828086748,   0.02005851806501,  0.00205861885564,  0.00302439095741,  -0.02950134983287 ),
    (0.33642304856132,  -1.49858979367799, -0.25572241425570,   0.87350271418188, -0.11828570177555,    0.12205022308084,  0.11921148675203,  -0.80774944671438, -0.07834489609479,    0.47854794562326, -0.00469977914380,  -0.12453458140019, -0.00589500224440,   -0.04067510197014,  0.05724228140351,   0.08333755284107,  0.00832043980773,  -0.04237348025746, -0.01635381384540,  0.02977207319925,  -0.01760176568150 ),
    (0.44915256608450,  -0.62820619233671, -0.14351757464547,   0.29661783706366, -0.22784394429749,   -0.37256372942400, -0.01419140100551,   0.00213767857124,  0.04078262797139,   -0.42029820170918, -0.12398163381748,   0.22199650564824,  0.04097565135648,    0.00613424350682,  0.10478503600251,   0.06747620744683, -0.01863887810927,   0.05784820375801, -0.03193428438915,  0.03222754072173,   0.00541907748707 ),
    (0.56619470757641,  -1.04800335126349, -0.75464456939302,   0.29156311971249,  0.16242137742230,   -0.26806001042947,  0.16744243493672,   0.00819999645858, -0.18901604199609,    0.45054734505008,  0.30931782841830,  -0.33032403314006, -0.27562961986224,    0.06739368333110,  0.00647310677246,  -0.04784254229033,  0.08647503780351,   0.01639907836189, -0.03788984554840,  0.01807364323573,  -0.00588215443421 ),
    (0.58100494960553,  -0.51035327095184, -0.53174909058578,  -0.31863563325245, -0.14289799034253,   -0.20256413484477,  0.17520704835522,   0.14728154134330,  0.02377945217615,    0.38952639978999,  0.15558449135573,  -0.23313271880868, -0.25344790059353,   -0.05246019024463,  0.01628462406333,  -0.02505961724053,  0.06920467763959,   0.02442357316099, -0.03721611395801,  0.01818801111503,  -0.00749618797172 ),
    (0.53648789255105,  -0.25049871956020, -0.42163034350696,  -0.43193942311114, -0.00275953611929,   -0.03424681017675,  0.04267842219415,  -0.04678328784242, -0.10214864179676,    0.26408300200955,  0.14590772289388,   0.15113130533216, -0.02459864859345,   -0.17556493366449, -0.11202315195388,  -0.18823009262115, -0.04060034127000,   0.05477720428674,  0.04788665548180,  0.04704409688120,  -0.02217936801134 )
  );

  ABButter: array[0..11, 0..2*BUTTER_ORDER + 1-1] of Float_t = (
    (0.99308203517541, -1.98611621154089, -1.98616407035082,  0.986211929160751, 0.99308203517541 ),
    (0.992472550461293,-1.98488843762334, -1.98494510092258,  0.979389350028798, 0.992472550461293),
    (0.989641019334721,-1.97917472731008, -1.97928203866944,  0.979389350028798, 0.989641019334721),
    (0.98621192462708, -1.97223372919527, -1.97242384925416,  0.97261396931306,  0.98621192462708 ),
    (0.98500175787242, -1.96977855582618, -1.97000351574484,  0.97022847566350,  0.98500175787242 ),
    (0.97938932735214, -1.95835380975398, -1.95877865470428,  0.95920349965459,  0.97938932735214 ),
    (0.97531843204928, -1.95002759149878, -1.95063686409857,  0.95124613669835,  0.97531843204928 ),
    (0.97316523498161, -1.94561023566527, -1.94633046996323,  0.94705070426118,  0.97316523498161 ),
    (0.96454515552826, -1.92783286977036, -1.92909031105652,  0.93034775234268,  0.96454515552826 ),
    (0.96009142950541, -1.91858953033784, -1.92018285901082,  0.92177618768381,  0.96009142950541 ),
    (0.95856916599601, -1.91542108074780, -1.91713833199203,  0.91885558323625,  0.95856916599601 ),
    (0.94597685600279, -1.88903307939452, -1.89195371200558,  0.89487434461664,  0.94597685600279 )
  );


// When calling these filter procedures, make sure that ip[-order] and op[-order] point to real data!

// If your compiler complains that "'operation on 'output' may be undefined", you can
// either ignore the warnings or uncomment the three "y" lines (and comment out the indicated line)

procedure filterYule(input, output: PFloat_t; nSamples: csize_t; kernel: PFloat_t);
begin
  while nSamples > 0 do begin
    output^ :=  1e-10  // 1e-10 is a hack to avoid slowdown because of denormals
                + input [0]  * kernel[0]
                - output[-1] * kernel[1]
                + input [-1] * kernel[2]
                - output[-2] * kernel[3]
                + input [-2] * kernel[4]
                - output[-3] * kernel[5]
                + input [-3] * kernel[6]
                - output[-4] * kernel[7]
                + input [-4] * kernel[8]
                - output[-5] * kernel[9]
                + input [-5] * kernel[10]
                - output[-6] * kernel[11]
                + input [-6] * kernel[12]
                - output[-7] * kernel[13]
                + input [-7] * kernel[14]
                - output[-8] * kernel[15]
                + input [-8] * kernel[16]
                - output[-9] * kernel[17]
                + input [-9] * kernel[18]
                - output[-10]* kernel[19]
                + input [-10]* kernel[20];
    inc(output);
    inc(input);
    dec(nSamples);
  end;
end;

procedure filterButter(input, output: PFloat_t; nSamples: csize_t; kernel: PFloat_t);
begin
  while nSamples > 0 do begin
    output^ := input [0]  * kernel[0]
               - output[-1] * kernel[1]
               + input [-1] * kernel[2]
               - output[-2] * kernel[3]
               + input [-2] * kernel[4];
    inc(output);
    inc(input);
    dec(nSamples);
  end;
end;


// returns TRUE if successful, FALSE if not
function ResetSampleFrequency(samplefreq: clong): boolean;
var i: cint;
begin
  // zero out initial values
  for i:=0 to MAX_ORDER-1 do begin
    linprebuf[i] := 0.0;
    lstepbuf[i] := 0.0;
    loutbuf[i] := 0.0;
    rinprebuf[i] := 0.0;
    rstepbuf[i] := 0.0;
    routbuf[i] := 0.0;
  end;

  case samplefreq of
    96000: freqindex := 0;
    88200: freqindex := 1;
    64000: freqindex := 2;
    48000: freqindex := 3;
    44100: freqindex := 4;
    32000: freqindex := 5;
    24000: freqindex := 6;
    22050: freqindex := 7;
    16000: freqindex := 8;
    12000: freqindex := 9;
    11025: freqindex := 10;
     8000: freqindex := 11;
    else begin
      Result := False;
      exit;
    end;
  end;//case

  sampleWindow := Ceil(samplefreq * RMS_WINDOW_TIME_MS / 1000.0);

  lsum := 0.0;
  rsum := 0.0;
  totsamp := 0;

  FillByte(A, Length(A)*SizeOf(Uint32_t), 0);

  Result := True;
end;

function InitGainAnalysis(samplefreq: clong): boolean;
begin
  Result := ResetSampleFrequency(samplefreq);
  if not Result then exit;

  linpre := @linprebuf[MAX_ORDER];
  rinpre := @rinprebuf[MAX_ORDER];
  lstep := @lstepbuf[MAX_ORDER];
  rstep := @rstepbuf[MAX_ORDER];
  lout := @loutbuf[MAX_ORDER];
  rout := @routbuf[MAX_ORDER];

  FillByte(B, Length(B)*SizeOf(Uint32_t), 0);

  Result := True;
end;

function fsqr(const d: double): double; inline;
begin
  Result := d*d;
end;

// memcpy(void *dest, const void *src, size_t n);
procedure memcpy(aDst, aSrc: PFloat_t; aCount: csize_t); inline;
begin
  Move(aSrc^, aDst^, aCount);
end;

// memmove(void *dst, const void *src, size_t n);
procedure memmove(aDst, aSrc: PFloat_t; aCount: csize_t); inline;
begin
  Move(aSrc^, aDst^, aCount);
end;

// returns TRUE if successful, FALSE if not
function AnalyzeSamples(left_samples, right_samples: PFloat_t;
                        num_samples: csize_t; num_channels: cint): boolean;
var curleft, curright: PFloat_t;
    batchsamples, cursamples, cursamplepos: clong;
    i: cint;
    val: double;
    ival: cint;
begin

  Result := False;
  if num_samples = 0 then exit;

  cursamplepos := 0;
  batchsamples := clong(num_samples);

  case num_channels of
    1: right_samples := left_samples;
    2: ;
    else exit;
  end;//case


  if num_samples < MAX_ORDER then begin
    memcpy(@linprebuf[MAX_ORDER], @left_samples[0], num_samples);
    memcpy(@rinprebuf[MAX_ORDER], @right_samples[0], num_samples);
  end else begin
    memcpy(@linprebuf[MAX_ORDER], @left_samples[0],  MAX_ORDER);
    memcpy(@rinprebuf[MAX_ORDER], @right_samples[0], MAX_ORDER);
  end;

  while batchsamples > 0 do begin
    if batchsamples > sampleWindow-totsamp
      then cursamples := sampleWindow - totsamp
      else cursamples := batchsamples;

    if cursamplepos < MAX_ORDER then begin
      curleft  := @linpre[cursamplepos];
      curright := @rinpre[cursamplepos];
      if cursamples > MAX_ORDER - cursamplepos then
        cursamples := MAX_ORDER - cursamplepos;
    end else begin
      curleft  := @left_samples[cursamplepos];
      curright := @right_samples[cursamplepos];
    end;


    filterYule( @curleft[0], @lstep[totsamp], cursamples, @ABYule[freqindex]);
    filterYule( @curright[0], @rstep[totsamp], cursamples, @ABYule[freqindex]);

    filterButter( @lstep[totsamp], @lout[totsamp], cursamples, @ABButter[freqindex]);
    filterButter( @rstep[totsamp], @rout[totsamp], cursamples, @ABButter[freqindex]);

    curleft := @lout[totsamp];   // Get the squared values
    curright := @rout[totsamp];

    i := cursamples Mod 16;
    while i > 0 do begin
      lsum += fsqr(curleft^);
      inc(curleft);
      rsum += fsqr(curright^);
      inc(curright);
      dec(i);
    end;

    i := cursamples div 16;
    while i > 0 do begin
      lsum += fsqr(curleft[0])
              + fsqr(curleft[1])
              + fsqr(curleft[2])
              + fsqr(curleft[3])
              + fsqr(curleft[4])
              + fsqr(curleft[5])
              + fsqr(curleft[6])
              + fsqr(curleft[7])
              + fsqr(curleft[8])
              + fsqr(curleft[9])
              + fsqr(curleft[10])
              + fsqr(curleft[11])
              + fsqr(curleft[12])
              + fsqr(curleft[13])
              + fsqr(curleft[14])
              + fsqr(curleft[15]);
      inc(curleft, 16);
      rsum += fsqr(curright[0])
              + fsqr(curright[1])
              + fsqr(curright[2])
              + fsqr(curright[3])
              + fsqr(curright[4])
              + fsqr(curright[5])
              + fsqr(curright[6])
              + fsqr(curright[7])
              + fsqr(curright[8])
              + fsqr(curright[9])
              + fsqr(curright[10])
              + fsqr(curright[11])
              + fsqr(curright[12])
              + fsqr(curright[13])
              + fsqr(curright[14])
              + fsqr(curright[15]);
      inc(curright, 16);
      dec(i);
    end;

    batchsamples -= cursamples;
    cursamplepos += cursamples;
    totsamp      += cursamples;
    if totsamp = sampleWindow then begin  // Get the Root Mean Square (RMS) for this set of samples
      val := STEPS_per_dB * 10.0 * log10((lsum+rsum) / totsamp * 0.5 + 1.e-37 );

      ival := Trunc(val);  // (int) val;
      if ival < 0 then ival := 0;
      if ival >= Length(A) then ival := Length(A) - 1;
      A[ival] := A[ival]+1;
      lsum := 0.0;
      rsum := 0.0;
      memmove(@loutbuf[0], @loutbuf[totsamp], MAX_ORDER);
      memmove(@routbuf[0], @routbuf[totsamp], MAX_ORDER);
      memmove(@lstepbuf[0], @lstepbuf[totsamp], MAX_ORDER);
      memmove(@rstepbuf[0], @rstepbuf[totsamp], MAX_ORDER);
      totsamp := 0;
    end;

    if totsamp > sampleWindow then exit;  // somehow I really screwed up: Error in programming! Contact author about totsamp > sampleWindow
  end;

    if num_samples < MAX_ORDER then begin
        memmove(@linprebuf[0], @linprebuf[num_samples], MAX_ORDER-num_samples);
        memmove(@rinprebuf[0], @rinprebuf[num_samples], MAX_ORDER-num_samples);
        memcpy(@linprebuf[MAX_ORDER - num_samples], @left_samples[0], num_samples);
        memcpy(@rinprebuf[MAX_ORDER - num_samples], @right_samples[0], num_samples);
    end
    else begin
        memcpy(@linprebuf[0], @left_samples[num_samples - MAX_ORDER], MAX_ORDER);
        memcpy(@rinprebuf[0], @right_samples[num_samples - MAX_ORDER], MAX_ORDER);
    end;

  Result := True;
end;


function analyzeResult(Arr: PUint32_t; len: csize_t): Float_t;
var elems: Uint32_t;
    upper: Int32_t;
    i: csize_t;
begin
  elems := 0;
  for i:=0 to len-1 do
    elems += Arr[i];

  if elems = 0 then begin
    Result := GAIN_NOT_ENOUGH_SAMPLES;
    exit;
  end;

  upper := Int32_t(Ceil(elems * (1.0 - RMS_PERCENTILE)));
  i := len;
  while i > 0 do begin // for ( i = len; i-- > 0; )
    dec(i);
    upper -= Arr[i];
    if upper <= 0 then break;
  end;

    Result :=Float_t(PINK_REF - i / STEPS_per_dB);
end;



function GetTitleGain: Float_t;
var i: integer;
begin
  Result := AnalyzeResult(@A[0], Length(A));

  for i:=0 to length(A)-1 do begin
    B[i] += A[i];
    A[i] := 0;
  end;

  for i:=0 to MAX_ORDER-1 do begin
    linprebuf[i] := 0.0;
    lstepbuf[i] := 0.0;
    loutbuf[i] := 0.0;
    rinprebuf[i] := 0.0;
    rstepbuf[i] := 0.0;
    routbuf[i] := 0.0;
  end;

    totsamp := 0;
    lsum := 0.0;
    rsum := 0.0;
end;

function GetAlbumGain: Float_t;
begin
  Result := AnalyzeResult(@B[0], Length(B));
end;


end.

