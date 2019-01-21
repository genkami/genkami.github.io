---
layout: post
title: Docker上のUbuntu Bionicでapt-get install tzdataできない問題
tags:
- Linux
---

``` dockerfile
# Dockerfile.xenial
FROM ubuntu:xenial
RUN apt-get update -y && apt-get install -y tzdata
```

```
$ docker build -t tzdata:xenial -f Dockerfile.xenial .
Sending build context to Docker daemon  2.048kB
Step 1/2 : FROM ubuntu:xenial
xenial: Pulling from library/ubuntu
b849b56b69e7: Pull complete
42986ef25bcd: Pull complete
d927c1b717ec: Pull complete
15b86ea20233: Pull complete
Digest: sha256:b967b9f2a5625231a22db642609e61b7b1a5481128f51fe771e91bb92e0a35d0
Status: Downloaded newer image for ubuntu:xenial
 ---> b0ef3016420a
Step 2/2 : RUN apt-get update -y && apt-get install -y tzdata
 ---> Running in 2d1d83e4481f
Get:1 http://archive.ubuntu.com/ubuntu xenial InRelease [247 kB]
Get:2 http://security.ubuntu.com/ubuntu xenial-security InRelease [107 kB]
Get:3 http://security.ubuntu.com/ubuntu xenial-security/main amd64 Packages [767 kB]
Get:4 http://archive.ubuntu.com/ubuntu xenial-updates InRelease [109 kB]
Get:5 http://archive.ubuntu.com/ubuntu xenial-backports InRelease [107 kB]
Get:6 http://archive.ubuntu.com/ubuntu xenial/main amd64 Packages [1558 kB]
Get:7 http://security.ubuntu.com/ubuntu xenial-security/restricted amd64 Packages [12.7 kB]
Get:8 http://security.ubuntu.com/ubuntu xenial-security/universe amd64 Packages [526 kB]
Get:9 http://security.ubuntu.com/ubuntu xenial-security/multiverse amd64 Packages [4026 B]
Get:10 http://archive.ubuntu.com/ubuntu xenial/restricted amd64 Packages [14.1 kB]
Get:11 http://archive.ubuntu.com/ubuntu xenial/universe amd64 Packages [9827 kB]
Get:12 http://archive.ubuntu.com/ubuntu xenial/multiverse amd64 Packages [176 kB]
Get:13 http://archive.ubuntu.com/ubuntu xenial-updates/main amd64 Packages [1166 kB]
Get:14 http://archive.ubuntu.com/ubuntu xenial-updates/restricted amd64 Packages [13.1 kB]
Get:15 http://archive.ubuntu.com/ubuntu xenial-updates/universe amd64 Packages [927 kB]
Get:16 http://archive.ubuntu.com/ubuntu xenial-updates/multiverse amd64 Packages [19.0 kB]
Get:17 http://archive.ubuntu.com/ubuntu xenial-backports/main amd64 Packages [7942 B]
Get:18 http://archive.ubuntu.com/ubuntu xenial-backports/universe amd64 Packages [8532 B]
Fetched 15.6 MB in 6s (2499 kB/s)
Reading package lists...
Reading package lists...
Building dependency tree...
Reading state information...
The following NEW packages will be installed:
  tzdata
0 upgraded, 1 newly installed, 0 to remove and 4 not upgraded.
Need to get 168 kB of archives.
After this operation, 2867 kB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu xenial-updates/main amd64 tzdata all 2018i-0ubuntu0.16.04 [168 kB]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 168 kB in 1s (121 kB/s)
Selecting previously unselected package tzdata.
(Reading database ... 4768 files and directories currently installed.)
Preparing to unpack .../tzdata_2018i-0ubuntu0.16.04_all.deb ...
Unpacking tzdata (2018i-0ubuntu0.16.04) ...
Setting up tzdata (2018i-0ubuntu0.16.04) ...
debconf: unable to initialize frontend: Dialog
debconf: (TERM is not set, so the dialog frontend is not usable.)
debconf: falling back to frontend: Readline
debconf: unable to initialize frontend: Readline
debconf: (Can't locate Term/ReadLine.pm in @INC (you may need to install the Term::ReadLine module) (@INC contains: /etc/perl /usr/local/lib/x86_64-linux-gnu/perl/5.22.1 /usr/local/share/perl/5.22.1 /usr/lib/x86_64-linux-gnu/perl5/5.22 /usr/share/perl5 /usr/lib/x86_64-linux-gnu/perl/5.22 /usr/share/perl/5.22 /usr/local/lib/site_perl /usr/lib/x86_64-linux-gnu/perl-base .) at /usr/share/perl5/Debconf/FrontEnd/Readline.pm line 7.)
debconf: falling back to frontend: Teletype

Current default time zone: 'Etc/UTC'
Local time is now:      Mon Jan 21 09:06:07 UTC 2019.
Universal Time is now:  Mon Jan 21 09:06:07 UTC 2019.
Run 'dpkg-reconfigure tzdata' if you wish to change it.

Removing intermediate container 2d1d83e4481f
 ---> 3d58807b367a
Successfully built 3d58807b367a
Successfully tagged tzdata:xenial
```


``` dockerfile
# Dockerfile.bionic
FROM ubuntu:bionic
RUN apt-get update -y && apt-get install -y tzdata
```

```
$ docker build -t tzdata:bionic -f Dockerfile.bionic .
Sending build context to Docker daemon  3.072kB
Step 1/2 : FROM ubuntu:bionic
 ---> 1d9c17228a9e
Step 2/2 : RUN apt-get update -y && apt-get install -y tzdata
 ---> Running in 38492b994a53
Get:1 http://security.ubuntu.com/ubuntu bionic-security InRelease [83.2 kB]
Get:2 http://archive.ubuntu.com/ubuntu bionic InRelease [242 kB]
Get:3 http://security.ubuntu.com/ubuntu bionic-security/universe amd64 Packages [139 kB]
Get:4 http://archive.ubuntu.com/ubuntu bionic-updates InRelease [88.7 kB]
Get:5 http://archive.ubuntu.com/ubuntu bionic-backports InRelease [74.6 kB]
Get:6 http://security.ubuntu.com/ubuntu bionic-security/multiverse amd64 Packages [1365 B]
Get:7 http://security.ubuntu.com/ubuntu bionic-security/main amd64 Packages [309 kB]
Get:8 http://archive.ubuntu.com/ubuntu bionic/multiverse amd64 Packages [186 kB]
Get:9 http://archive.ubuntu.com/ubuntu bionic/universe amd64 Packages [11.3 MB]
Get:10 http://archive.ubuntu.com/ubuntu bionic/main amd64 Packages [1344 kB]
Get:11 http://archive.ubuntu.com/ubuntu bionic/restricted amd64 Packages [13.5 kB]
Get:12 http://archive.ubuntu.com/ubuntu bionic-updates/restricted amd64 Packages [10.7 kB]
Get:13 http://archive.ubuntu.com/ubuntu bionic-updates/multiverse amd64 Packages [6933 B]
Get:14 http://archive.ubuntu.com/ubuntu bionic-updates/main amd64 Packages [635 kB]
Get:15 http://archive.ubuntu.com/ubuntu bionic-updates/universe amd64 Packages [907 kB]
Get:16 http://archive.ubuntu.com/ubuntu bionic-backports/universe amd64 Packages [3666 B]
Fetched 15.4 MB in 7s (2309 kB/s)
Reading package lists...
Reading package lists...
Building dependency tree...
Reading state information...
The following NEW packages will be installed:
  tzdata
0 upgraded, 1 newly installed, 0 to remove and 16 not upgraded.
Need to get 189 kB of archives.
After this operation, 3104 kB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu bionic-updates/main amd64 tzdata all 2018i-0ubuntu0.18.04 [189 kB]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 189 kB in 1s (143 kB/s)
Selecting previously unselected package tzdata.
(Reading database ... 4039 files and directories currently installed.)
Preparing to unpack .../tzdata_2018i-0ubuntu0.18.04_all.deb ...
Unpacking tzdata (2018i-0ubuntu0.18.04) ...
Setting up tzdata (2018i-0ubuntu0.18.04) ...
debconf: unable to initialize frontend: Dialog
debconf: (TERM is not set, so the dialog frontend is not usable.)
debconf: falling back to frontend: Readline
debconf: unable to initialize frontend: Readline
debconf: (Can't locate Term/ReadLine.pm in @INC (you may need to install the Term::ReadLine module) (@INC contains: /etc/perl /usr/local/lib/x86_64-linux-gnu/perl/5.26.1 /usr/local/share/perl/5.26.1 /usr/lib/x86_64-linux-gnu/perl5/5.26 /usr/share/perl5 /usr/lib/x86_64-linux-gnu/perl/5.26 /usr/share/perl/5.26 /usr/local/lib/site_perl /usr/lib/x86_64-linux-gnu/perl-base) at /usr/share/perl5/Debconf/FrontEnd/Readline.pm line 7.)
debconf: falling back to frontend: Teletype
Configuring tzdata
------------------

Please select the geographic area in which you live. Subsequent configuration
questions will narrow this down by presenting a list of cities, representing
the time zones in which they are located.

  1. Africa      4. Australia  7. Atlantic  10. Pacific  13. Etc
  2. America     5. Arctic     8. Europe    11. SystemV
  3. Antarctica  6. Asia       9. Indian    12. US
Geographic area:
Use of uninitialized value $_[1] in join or string at /usr/share/perl5/Debconf/DbDriver/Stack.pm line 111.

Current default time zone: '/UTC'
Local time is now:      Mon Jan 21 09:07:09 UTC 2019.
Universal Time is now:  Mon Jan 21 09:07:09 UTC 2019.
Run 'dpkg-reconfigure tzdata' if you wish to change it.

Use of uninitialized value $val in substitution (s///) at /usr/share/perl5/Debconf/Format/822.pm line 83, <GEN6> line 4.
Use of uninitialized value $val in concatenation (.) or string at /usr/share/perl5/Debconf/Format/822.pm line 84, <GEN6> line 4.
Removing intermediate container 38492b994a53
 ---> 8b1d2ec57c28
Successfully built 8b1d2ec57c28
Successfully tagged tzdata:bionic
```


