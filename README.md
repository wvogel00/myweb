# myweb

## 動作確認環境
MacOS Catalina 10.15.6

## 仕様
- サイトページの表示
- ログイン機能で，ブラウザ上でのコンテンツ編集

### postgresql (備忘録)
#### インストール
```
brew install postgresql
```
#### 指示に従いpath設定
```
echo 'export PATH="/usr/local/opt/krb5/bin:$PATH"' >> /Users/yourname/.bash_profile
echo 'export PATH="/usr/local/opt/krb5/sbin:$PATH"' >> /Users/yourname/.bash_profile

export LDFLAGS="-L/usr/local/opt/krb5/lib"
export CPPFLAGS="-I/usr/local/opt/krb5/include"

export PKG_CONFIG_PATH="/usr/local/opt/krb5/lib/pkgconfig"
```

#### 初期設定
```
brew postinstall postgresql
initdb /usr/local/var/postgres -E utf8 # utf8で初期化
brew services start postgresql # 軌道
psql -V # psql (PostgreSQL) 12.4.
psql -l # 中身を確認
```
#### 操作
- ユーザーの作成 ... ```createuser -P test```
- DBの作成 ... ```createdb testdb -O test```
- テーブルの作成 ... ```create table test (id int, title text, remark text);```
- レコードの挿入 ... ```INSERT INTO test values (1,'タイトル', '備考');```
- 選択 ... ```select * from test``` ，```select * from test where id=1;``` など
- レコード数を取得 ... ```select count (*) test;```