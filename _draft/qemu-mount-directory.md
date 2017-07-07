qemu-mount-directory

-hda fat:path/to/dir でディレクトリを仮想的なFATファイルシステムのボリュームとして扱うことができる
デフォルトは読み込み専用。書き込みもしたいときは
-hda fat:rw:path/to/dir
qemuの起動後にホスト側でディレクトリに変更を加えても検知されないので注意。