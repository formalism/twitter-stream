twitter-stream
===============

## Overview

This application streams tweets with specified keyword(s).

## Prerequisites

You have to create your application first on [this page](https://apps.twitter.com/app/new).
Next obtain the following keys from the application's page.

- Consumer Key
- Consumer Secret
- Access Token
- Access Token Secret

These keys should be found in "Keys and Access Tokens" tab. Access Token and Access Token Secret
should be also generated by pressing the button on the page.

## How to build

- git clone
- Modify the Main.hs with your key.
- stack build

## References

Almost the same as the following. Just [Stack](https://docs.haskellstack.org/en/stable/README/) Ready.

[Conduit で Twitter Streaming API を扱う](http://krdlab.hatenablog.com/entry/20120521/1337529701)

## Author

[Tomoyuki Ueda](https://github.com/formalism)
