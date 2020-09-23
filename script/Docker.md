## Docker 使用说明
1. git clone https://github.com/dromara/soul.git
2. 修改配置文件
> 修改soul-admin/src/main/resources/application-local.yaml文件，修改mysql配置  
修改soul-bootstrap/src/main/resources/application-local.yaml，修改websocket url配置，把127.0.0.1，改成你宿主机实际ip
3. 回到soul根目录下，执行mvn clean package打包
4. 去script目录下，执行docker-compose up -d，会开始build镜像，成功后，会成功启动soul-admin和soul-bootstrap
5. http://IP:9095/    帐号信息admin/123456 登录即可
    