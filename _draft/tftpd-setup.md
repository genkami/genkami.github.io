http://tftpd32.jounin.net/tftpd32_download.html
tftpd64をダウンロード＆インストール
Settings -> GLOBAL -> Start Services から、TFTP Server以外のチェックを外す
再起動しろと言われて再起動したら設定がデフォルトに戻ってる
そんなときはini直接編集
.iniファイルへのアクセスが拒否されているのが原因
管理者として実行したらいける