# Salmon - Media Server
Local media server utilizing ffmpeg to produce and serve HLS streams for music files (mpeg-dash and webrtc to come). Part of the [Salmon](https://github.com/steams/salmon) streaming platform.

This program is intended to be installed on the machine where your media is stored and will watch a given folder for current media, updates and deletions, and synchronize the media listed in Salmon Hub and displayed in Salmon Clients with the state of your filesystem. This program generates HLS playlists and segments for each piece of media within the watched folder and makes them available for streaming over http.

## Usage

Run the program from the command line in the folder your media is stored

```bash
Usage: salmon-media-server (-u|--username USERNAME) (-p|--password PASSWORD)

Available options:
  -u,--username USERNAME   User account name for Salmon Hub
  -p,--password PASSWORD   User password name for Salmon Hub
  -h,--help                Show this help text


```

Then log into a web or desktop salmon client to view and play your media from any device.
