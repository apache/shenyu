package org.dromara.soul.client.alibaba.dubbo.validation.mock;

import com.alibaba.dubbo.validation.MethodValidated;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * Mock Validator Target.
 *
 * @author David Liu
 */
public interface MockValidatorTarget {
    /**
     * mock method: method1.
     *
     * @param anything parameter
     */
    @MethodValidated
    void method1(String anything);
    
    /**
     * mock method: method2.
     *
     * @param mockValidationParameter parameter
     */
    @MethodValidated
    void method2(@NotNull @MockConstraint MockValidationParameter mockValidationParameter);
    
    /**
     * mock method: method3.
     *
     * @param parameters parameter
     */
    void method3(MockValidationParameter[] parameters);
    
    /**
     * mock method: method4.
     *
     * @param strings parameter
     */
    void method4(List<String> strings);
    
    /**
     * mock method: method5.
     *
     * @param map parameter
     */
    void method5(Map<String, String> map);
    
    interface Method1 {
    
    }
    
    interface Method2 {
    
    }
    
    interface Method3 {
    
    }
    
    interface Method4 {
    
    }
    
    interface Method5 {
    
    }
}
