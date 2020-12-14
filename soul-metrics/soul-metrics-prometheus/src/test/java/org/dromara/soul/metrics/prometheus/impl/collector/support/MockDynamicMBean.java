package org.dromara.soul.metrics.prometheus.impl.collector.support;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.DynamicMBean;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ReflectionException;
import java.lang.reflect.Constructor;

/**
 * Mock Dynamic MBean.
 *
 * @author David Liu
 */
public class MockDynamicMBean implements DynamicMBean {
    private final MBeanInfo mBeanInfo;
    
    private final String className;
    
    private String name;
    
    public MockDynamicMBean(final String name, final boolean nameFiledReadable) {
        this.name = name;
        className = this.getClass().getName();
        final String description = "Simple implementation of a MBean.";
        
        MBeanAttributeInfo[] attributes = new MBeanAttributeInfo[1];
        attributes[0] = new MBeanAttributeInfo("Name", "java.lang.String", "Name: name string.", nameFiledReadable, true, false);
        MBeanConstructorInfo[] constructors = new MBeanConstructorInfo[1];
        Constructor[] classConstructors = this.getClass().getConstructors();
        constructors[0] = new MBeanConstructorInfo("HelloDynamic(): Constructs a HelloDynamic object", classConstructors[0]);
        MBeanOperationInfo[] operations = new MBeanOperationInfo[1];
        operations[0] = new MBeanOperationInfo("print", "print(): print the name", null, "void", MBeanOperationInfo.INFO);
        MBeanNotificationInfo[] mBeanNotificationInfoArray = new MBeanNotificationInfo[0];
        mBeanInfo = new MBeanInfo(className, description, attributes, constructors, operations, mBeanNotificationInfoArray);
    }
    
    @Override
    public Object getAttribute(final String attribute) {
        if (attribute == null) {
            return null;
        }
        if ("Name".equals(attribute)) {
            return name;
        }
        return null;
    }
    
    @Override
    public void setAttribute(final Attribute attribute) {
        if (attribute == null) {
            return;
        }
        
        String attributeName = attribute.getName();
        Object value = attribute.getValue();
        try {
            if ("Name".equals(attributeName)) {
                if (value == null) {
                    name = null;
                } else if (String.class.isAssignableFrom(value.getClass())) {
                    name = (String) value;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    @Override
    public AttributeList getAttributes(final String[] attributeNames) {
        if (attributeNames == null) {
            return null;
        }
        
        AttributeList resultList = new AttributeList();
        if (attributeNames.length == 0) {
            return resultList;
        }
        
        for (String attributeName : attributeNames) {
            try {
                Object value = getAttribute(attributeName);
                resultList.add(new Attribute(attributeName, value));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return resultList;
    }
    
    @Override
    public AttributeList setAttributes(final AttributeList attributes) {
        if (attributes == null) {
            return null;
        }
        
        AttributeList resultList = new AttributeList();
        if (attributes.isEmpty()) {
            return resultList;
        }
        
        for (Object attribute : attributes) {
            Attribute attr = (Attribute) attribute;
            try {
                setAttribute(attr);
                String name = attr.getName();
                Object value = getAttribute(name);
                resultList.add(new Attribute(name, value));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return resultList;
    }
    
    @Override
    public Object invoke(final String operationName, final Object[] params, final String[] signature) throws ReflectionException {
        if ("print".equals(operationName)) {
            return "MBean name is " + name;
        } else {
            throw new ReflectionException(new NoSuchMethodException(operationName), "Cannot find the operation " + operationName + " in " + className);
        }
    }
    
    /**
     * get MBeanInfo.
     *
     * @return MBeanInfo
     */
    public MBeanInfo getMBeanInfo() {
        return mBeanInfo;
    }
}
