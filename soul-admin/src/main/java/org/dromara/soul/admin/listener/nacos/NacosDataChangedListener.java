package org.dromara.soul.admin.listener.nacos;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;

import com.alibaba.nacos.api.config.ConfigService;
import com.google.common.collect.Maps;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/**
 * © 2015-2020 Chenxj Copyright
 * 类    名：NacosDataChangedListener
 * 类 描 述：
 * 作    者：Chenxj
 * 邮    箱：chenios@foxmail.com
 * 日    期：2020年3月12日-上午10:00:14
 */
public class NacosDataChangedListener implements DataChangedListener{
	private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();
	private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();
	private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();
	private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();
	private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();
	private static final Comparator<SelectorData> selectorCp=Comparator.comparing(SelectorData::getSort);
	private static final Comparator<RuleData> ruleCp=Comparator.comparing(RuleData::getSort);
	private static final String group="DEFAULT_GROUP";
	private static final String pluginDataId="soul.plugin.json";
	private static final String selectorDataId="soul.selector.json";
	private static final String ruleDataId="soul.rule.json";
	private static final String authDataId="soul.auth.json";
	private static final String metaDataId="soul.meta.json";
	private final ConfigService configService;
	
	public NacosDataChangedListener(ConfigService configService) {
		this.configService=configService;
	}
	private void updateAuthMap(Gson gson,String configInfo,ErrorAction ea) {
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
	private void updatePluginMap(Gson gson,String configInfo,ErrorAction ea) {
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
	private void updateSelectorMap(Gson gson,String configInfo,ErrorAction ea) {
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
	private void updateMetaDataMap(Gson gson,String configInfo,ErrorAction ea) {
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
	private void updateRuleMap(Gson gson,String configInfo,ErrorAction ea) {
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
	
	private String getConfig(String dataId) {
		try {
			return configService.getConfig(dataId, group, 6000);
		}catch (Exception e) {
			return "{}";
		}
	}
	private void publishConfig(Gson gson,String dataId,Object data) {
		try {
			configService.publishConfig(dataId, group, gson.toJson(data));
		}catch (Exception e) {
		}
	}
	
	@Override
	public void onAppAuthChanged(List<AppAuthData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateAuthMap(gson, getConfig(authDataId), null);
		changed.stream().forEach(appAuth->{
			switch (eventType) {
			case DELETE:
				AUTH_MAP.remove(appAuth.getAppKey());
				break;
			default:
				AUTH_MAP.put(appAuth.getAppKey(), appAuth);
				break;
			}
		});
		publishConfig(gson, authDataId, AUTH_MAP);
	}
	@Override
	public void onPluginChanged(List<PluginData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updatePluginMap(gson, getConfig(pluginDataId), null);
		changed.stream().forEach(plugin->{
			switch (eventType) {
			case DELETE:
				PLUGIN_MAP.remove(plugin.getName());
				break;
			default:
				PLUGIN_MAP.put(plugin.getName(), plugin);
				break;
			}
		});
		publishConfig(gson, pluginDataId, PLUGIN_MAP);
	}
	@Override
	public void onSelectorChanged(List<SelectorData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateSelectorMap(gson, getConfig(selectorDataId), null);
		changed.stream().forEach(selector->{
			List<SelectorData>ls=null;
			switch (eventType) {
			case DELETE:
				ls=SELECTOR_MAP
				.getOrDefault(selector.getPluginName(), new ArrayList<>())
				.stream()
				.filter(s->!s.getId().equals(selector.getId()))
				.collect(Collectors.toList());
				break;
			default:
				ls=SELECTOR_MAP
				.getOrDefault(selector.getPluginName(), new ArrayList<>())
				.stream()
				.filter(s->!s.getId().equals(selector.getId()))
				.collect(Collectors.toList());
				ls.add(selector);
				break;
			}
			SELECTOR_MAP.put(selector.getPluginName(), ls.stream().sorted(selectorCp).collect(Collectors.toList()));
		});
		publishConfig(gson, selectorDataId, SELECTOR_MAP);
	}
	@Override
	public void onMetaDataChanged(List<MetaData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateMetaDataMap(gson, getConfig(metaDataId), null);
		changed.stream().forEach(meta->{
			switch (eventType) {
			case DELETE:
				META_DATA.remove(meta.getPath());
				break;
			default:
				META_DATA.put(meta.getPath(), meta);
				break;
			}
		});
		publishConfig(gson, metaDataId, META_DATA);
	}
	@Override
	public void onRuleChanged(List<RuleData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateRuleMap(gson, getConfig(ruleDataId), null);
		changed.stream().forEach(rule->{
			List<RuleData>ls=null;
			switch (eventType) {
			case DELETE:
				ls=RULE_MAP
				.getOrDefault(rule.getSelectorId(), new ArrayList<>())
				.stream()
				.filter(s->!s.getId().equals(rule.getSelectorId()))
				.collect(Collectors.toList());
				break;
			default:
				ls=RULE_MAP
				.getOrDefault(rule.getSelectorId(), new ArrayList<>())
				.stream()
				.filter(s->!s.getId().equals(rule.getSelectorId()))
				.collect(Collectors.toList());
				ls.add(rule);
				break;
			}
			RULE_MAP.put(rule.getSelectorId(), ls.stream().sorted(ruleCp).collect(Collectors.toList()));
		});
		publishConfig(gson, ruleDataId, RULE_MAP);
	}
	
	private static interface ErrorAction{
		public void act(Object o);
	}
}
