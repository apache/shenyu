package org.dromara.soul.web.cache;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Executor;

import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.google.common.collect.Maps;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/**
 * NacosCacheHandler
 * 
 * @author Chenxj
 * @date 2020年3月12日-下午2:07:12
 */
public class NacosCacheHandler extends CommonCacheHandler {
	protected static final String group="DEFAULT_GROUP";
	protected static final String pluginDataId="soul.plugin.json";
	protected static final String selectorDataId="soul.selector.json";
	protected static final String ruleDataId="soul.rule.json";
	protected static final String authDataId="soul.auth.json";
	protected static final String metaDataId="soul.meta.json";
	protected final ConfigService configService;
	protected final Map<String, List<Listener>>listeners=Maps.newConcurrentMap();
	
	public NacosCacheHandler(final ConfigService configService) {
		this.configService=configService;
	}
	protected void updateAuthMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>newSet=new HashSet<>();
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				newSet.add(e.getKey());
				AUTH_MAP.put(e.getKey(), gson.fromJson(e.getValue(), AppAuthData.class));
			}
			Set<String>set=new HashSet<>(AUTH_MAP.keySet());
			set.removeAll(newSet);
			//移除AUTH_MAP中多余keys
			if(!set.isEmpty()) {
				set.forEach(k->AUTH_MAP.remove(k));
			}
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	protected void updatePluginMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>newSet=new HashSet<>();
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				newSet.add(e.getKey());
				PLUGIN_MAP.put(e.getKey(), gson.fromJson(e.getValue(), PluginData.class));
			}
			Set<String>set=new HashSet<>(PLUGIN_MAP.keySet());
			set.removeAll(newSet);
			//移除PLUGIN_MAP中多余keys
			if(!set.isEmpty()) {
				set.forEach(k->PLUGIN_MAP.remove(k));
			}
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	protected void updateSelectorMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>newSet=new HashSet<>();
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				List<SelectorData>ls=new ArrayList<>();
				e.getValue().getAsJsonArray().forEach(je->ls.add(gson.fromJson(je, SelectorData.class)));
				newSet.add(e.getKey());
				SELECTOR_MAP.put(e.getKey(), ls);
			}
			Set<String>set=new HashSet<>(SELECTOR_MAP.keySet());
			set.removeAll(newSet);
			//移除SELECTOR_MAP中多余keys
			if(!set.isEmpty()) {
				set.forEach(k->SELECTOR_MAP.remove(k));
			}
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	protected void updateMetaDataMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>newSet=new HashSet<>();
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				newSet.add(e.getKey());
				META_DATA.put(e.getKey(), gson.fromJson(e.getValue(), MetaData.class));
			}
			Set<String>set=new HashSet<>(META_DATA.keySet());
			set.removeAll(newSet);
			//移除AUTH_MAP中多余keys
			if(!set.isEmpty()) {
				set.forEach(k->META_DATA.remove(k));
			}
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	protected void updateRuleMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>newSet=new HashSet<>();
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				List<RuleData>ls=new ArrayList<>();
				e.getValue().getAsJsonArray().forEach(je->ls.add(gson.fromJson(je, RuleData.class)));
				newSet.add(e.getKey());
				RULE_MAP.put(e.getKey(), ls);
			}
			Set<String>set=new HashSet<>(RULE_MAP.keySet());
			set.removeAll(newSet);
			//移除RULE_MAP中多余keys
			if(!set.isEmpty()) {
				set.forEach(k->RULE_MAP.remove(k));
			}
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	
	protected String getConfig(String dataId) {
		try {
			return configService.getConfig(dataId, group, 6000);
		}catch (Exception e) {
			return "{}";
		}
	}
	protected String getConfigAndSignListener(String dataId,Listener listener) {
		try {
			return configService.getConfigAndSignListener(dataId, group, 6000, listener);
		}catch (Exception e) {
			return "{}";
		}
	}
	protected void publishConfig(Gson gson,String dataId,Object data) {
		try {
			configService.publishConfig(dataId, group, gson.toJson(data));
		}catch (Exception e) {
		}
	}
	protected String buildMetaKey(MetaData meta) {
		return meta.getAppName()+'-'+meta.getServiceName()+meta.getMethodName();
	}
	protected void watcherData(String dataId,OnChange oc) {
		try {
			Listener listener=new Listener() {
				@Override
				public void receiveConfigInfo(String configInfo) {
					oc.change(configInfo);
				}
				@Override
				public Executor getExecutor() {
					return null;
				}
			};
			oc.change(getConfigAndSignListener(dataId, listener));
			listeners
			.getOrDefault(dataId, new ArrayList<>())
			.add(listener);
		}catch (Exception e) {
		}
	}
	protected static interface ErrorAction{
		public void act(Object o);
	}
	protected static interface OnChange{
		public void change(String changeData);
	}
}
