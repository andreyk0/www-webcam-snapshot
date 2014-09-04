www-webcam-snapshot
====

```
$ www-webcam-snapshot --help
Periodically grabs frames from attached USB webcams and serves them as JPG.

www-webcam-snapshot [OPTIONS]
  Usage: www-webcam-snapshot [--listen 1234] [--video-device /dev/video0]
  [--video-device /dev/video1]

Common flags:
  -l --listen=INT        local port to bind to, defaults to 9091
  -v --videodevice=ITEM  video device paths to grab frames from, defaults to
                         /dev/video*
  -i --interval=INT      refresh interval (seconds), defaults to 3
  -? --help              Display help message
  -V --version           Print version information

