package org.apache.shenyu.client.grpc;

public class TestRequest {
    private String name;

    public TestRequest(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
