# Give Your Voice  
A free voice recording software to ease the production of audio books.  
License: GPLv3  
Author: Lulu  
![icon](https://github.com/Lulu04/Give-Your-Voice/blob/main/Design/Logo/logo-final256.png)

> You can support the development by [donating](https://www.paypal.com/donate/?hosted_button_id=GZAR296S5LYBG)

# Target platform
Windows 64b (tested on Win10)  
Linux 64b (tested on Ubuntu 22.04 LTS)  
MacOS: to do  

# Features
- One directory per section (chapter/text/poem) -> your project is well organized.
- Allow multiple recording per section -> the software keep the page number where you stopped.
- Remove background noise on recording files.
- While recording, use **CONTROL KEY** to mark places where you have made reading errors, making it easy to find and correct them.
- Audio tools:
    - add silence, cut or silencing part
    - insert new recording at cursor position
    - replace the selected audio by a new recording
- Mix your voices recording in a simplified window
    - compressor, amplify and bass boost effects for voice enhancement
    - Add musics and sounds to your reading. Trim the musics/sounds and keep only the part that interests you
    - graphical volume envelope to easily control the volume (fadein, fadeout)
    - editable metadata
    - integrated audio limiter to minimize audio clipping
- Produce MP3 files compatible with the platform [**LibriVox**](https://wiki.librivox.org/index.php?title=Main_Page) and [**Litterature audio.com**](https://www.litteratureaudio.com/)
    - respects of their rules for file name
    - MP3 generated at 44100Hz, mono, constant bitrate mode, 128kbps
    - normalization at 89dB as MP3Gain, (can be modified)
- Export several MP3 files into a single ZIP file.
- Compress your terminated projects to free disk space.
- Full undo/redo system for audio modification.
- Supported language: French, English.
- This software was made with love, however it may contain some bugs!

# Screenshots
The main window with the project tree view and the audio edition at the bottom
![The main window with the project tree view and the audio edition at the bottom](https://github.com/Lulu04/Give-Your-Voice/blob/main/screenshot/01main.png)
  
Background noise analysis before recording voice  
![Background noise analisys before recording voice](https://github.com/Lulu04/Give-Your-Voice/blob/main/screenshot/02recording.png)
  
The mixer window where you produce the MP3 files
![The mixer window where you produce the MP3 files](https://github.com/Lulu04/Give-Your-Voice/blob/main/screenshot/03mixer.png)  
  
# How to compile
> At this time, the software works only for Windows 64b and Linux 64b.

You must have Lazarus IDE installed on your system with BGRABitmap package.  
- create a directory named GYV on your disk
- in 'GYV' directory clone this repository. You can also download the zip.
- again in 'GYV' directory, clone the repository [UnitsCommon](https://github.com/Lulu04/UnitsCommon). If you choose to download the zip, unzip the file and rename the obtained directory to "UnitsCommon".
- and again, in the 'GYV' directory, clone the repository [ALSound library](https://github.com/Lulu04/ALSound). If you choose to download the zip, unzip the file and rename the obtained directory to "ALSound".

The directory structure should be like:  
**\.GYV**  
**\.\.\. ALSound**  
**\.\.\. Give-Your-Voice**  
**\.\.\. UnitsCommon**  

Now, you are able to open the Lazarus project in the directory "Give-Your-Voice".  
