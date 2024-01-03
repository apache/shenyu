package org.apache.shenyu.client.grpc;

public class TestResponse {
    private String name;

    public TestResponse(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
