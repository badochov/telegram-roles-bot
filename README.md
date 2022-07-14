# telegram-roles-bot
Telegram bot bringing roles feature to your telegram chat

## Prerequisites
1. [Stack](https://docs.haskellstack.org/en/stable/README/)

## Build
1. Copy `app/Secret.hs.tmpl` to `app/Secret.hs`
2. Fill botKey, botUserName, botWebhookIp in `app/Secret.hs`
3. `$ stack --local-bin-path <path/to/dest/folder> install`

## Run
```shell
$ cd  <path/to/dest/folder>
$ ./telegram-roles-bot-exe
```
