# Nginx + Python サンプル

## 概要

Docker Composeを用いて，Nginxを用いた静的なページの配信と，Pythonを用いたHTML生成を同時に行う．

## 動かし方

### インストール

Dockerは[公式ページ](https://www.docker.com/)からダウンロード＆インストールできます．

Docker Composeは，[解説ページ](http://docs.docker.jp/compose/install.html)によると，以下のコマンドを実行するとインストールできるようです．

```
curl -L https://github.com/docker/compose/releases/download/1.6.2/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
```

### 実行

`docker-compose.yml` があるディレクトリで， `docker-compose up -d` を実行するとコンテナが立ち上がります．

ブラウザから `http://localhost/` にアクセスするとトップページが開きます．

Pythonのスクリプトが `http://localhost/sample.html` の内容を10秒に1回書き換えます．

終了するときは `docker-compose down` を実行します．
