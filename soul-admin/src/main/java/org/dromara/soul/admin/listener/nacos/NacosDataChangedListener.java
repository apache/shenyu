package org.dromara.soul.admin.listener.nacos;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
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
 * Use nacos to push data changes.
 *
 * @author chenxj
 * @date 2020年3月12日-上午10:00:14
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
			Set<String>set=new HashSet<>(AUTH_MAP.keySet());
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				set.remove(e.getKey());
				AUTH_MAP.put(e.getKey(), gson.fromJson(e.getValue(), AppAuthData.class));
			}
			AUTH_MAP.keySet().removeAll(set);
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	private void updatePluginMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>set=new HashSet<>(PLUGIN_MAP.keySet());
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				set.remove(e.getKey());
				PLUGIN_MAP.put(e.getKey(), gson.fromJson(e.getValue(), PluginData.class));
			}
			PLUGIN_MAP.keySet().removeAll(set);
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	private void updateSelectorMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>set=new HashSet<>(SELECTOR_MAP.keySet());
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				set.remove(e.getKey());
				List<SelectorData>ls=new ArrayList<>();
				e.getValue().getAsJsonArray().forEach(je->ls.add(gson.fromJson(je, SelectorData.class)));
				SELECTOR_MAP.put(e.getKey(), ls);
			}
			SELECTOR_MAP.keySet().removeAll(set);
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	private void updateMetaDataMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>set=new HashSet<>(META_DATA.keySet());
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				set.remove(e.getKey());
				META_DATA.put(e.getKey(), gson.fromJson(e.getValue(), MetaData.class));
			}
			META_DATA.keySet().removeAll(set);
		}catch (Exception e) {
			if(ea!=null) {
				ea.act(e);
			}
		}
	}
	private void updateRuleMap(Gson gson,String configInfo,ErrorAction ea) {
		try {
			JsonObject jo=gson.fromJson(configInfo, JsonObject.class);
			Set<String>set=new HashSet<>(RULE_MAP.keySet());
			for(Entry<String, JsonElement>e:jo.entrySet()) {
				set.remove(e.getKey());
				List<RuleData>ls=new ArrayList<>();
				e.getValue().getAsJsonArray().forEach(je->ls.add(gson.fromJson(je, RuleData.class)));
				RULE_MAP.put(e.getKey(), ls);
			}
			RULE_MAP.keySet().removeAll(set);
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
		switch (eventType) {
		case DELETE:
			changed.stream().forEach(appAuth->AUTH_MAP.remove(appAuth.getAppKey()));
			break;
		case REFRESH:
		case MYSELF:
			Set<String>set=new HashSet<>(AUTH_MAP.keySet());
			changed.stream().forEach(appAuth->{
				set.remove(appAuth.getAppKey());
				AUTH_MAP.put(appAuth.getAppKey(), appAuth);
			});
			AUTH_MAP.keySet().removeAll(set);
			break;
		default:
			changed.stream().forEach(appAuth->AUTH_MAP.put(appAuth.getAppKey(), appAuth));
			break;
		}
		publishConfig(gson, authDataId, AUTH_MAP);
	}
	@Override
	public void onPluginChanged(List<PluginData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updatePluginMap(gson, getConfig(pluginDataId), null);
		switch (eventType) {
		case DELETE:
			changed.stream().forEach(plugin->PLUGIN_MAP.remove(plugin.getName()));
			break;
		case REFRESH:
		case MYSELF:
			Set<String>set=new HashSet<>(PLUGIN_MAP.keySet());
			changed.stream().forEach(plugin->{
				set.remove(plugin.getName());
				PLUGIN_MAP.put(plugin.getName(), plugin);
			});
			PLUGIN_MAP.keySet().removeAll(set);
			break;
		default:
			changed.stream().forEach(plugin->PLUGIN_MAP.put(plugin.getName(), plugin));
			break;
		}
		publishConfig(gson, pluginDataId, PLUGIN_MAP);
	}
	@Override
	public void onSelectorChanged(List<SelectorData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateSelectorMap(gson, getConfig(selectorDataId), null);
		switch (eventType) {
		case DELETE:
			changed.stream().forEach(selector->{
				List<SelectorData>ls=SELECTOR_MAP
						.getOrDefault(selector.getPluginName(), new ArrayList<>())
						.stream()
						.filter(s->!s.getId().equals(selector.getId()))
						.sorted(selectorCp)
						.collect(Collectors.toList());
				SELECTOR_MAP.put(selector.getPluginName(), ls);
			});
			break;
		case REFRESH:
		case MYSELF:
			Set<String>set=new HashSet<>(SELECTOR_MAP.keySet());
			changed.stream().forEach(selector->{
				set.remove(selector.getPluginName());
				List<SelectorData>ls=SELECTOR_MAP
						.getOrDefault(selector.getPluginName(), new ArrayList<>())
						.stream()
						.sorted(selectorCp)
						.collect(Collectors.toList());
				SELECTOR_MAP.put(selector.getPluginName(), ls);
			});
			SELECTOR_MAP.keySet().removeAll(set);
			break;
		default:
			changed.stream().forEach(selector->{
				List<SelectorData>ls=SELECTOR_MAP
						.getOrDefault(selector.getPluginName(), new ArrayList<>())
						.stream()
						.filter(s->!s.getId().equals(selector.getId()))
						.sorted(selectorCp)
						.collect(Collectors.toList());
				ls.add(selector);
				SELECTOR_MAP.put(selector.getPluginName(), ls);
			});
			break;
		}
		publishConfig(gson, selectorDataId, SELECTOR_MAP);
	}
	@Override
	public void onMetaDataChanged(List<MetaData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateMetaDataMap(gson, getConfig(metaDataId), null);
		switch (eventType) {
		case DELETE:
			changed.stream().forEach(meta->META_DATA.remove(meta.getPath()));
			break;
		case REFRESH:
		case MYSELF:
			Set<String>set=new HashSet<>(META_DATA.keySet());
			changed.stream().forEach(meta->{
				set.remove(meta.getPath());
				META_DATA.put(meta.getPath(), meta);
			});
			META_DATA.keySet().removeAll(set);
			break;
		default:
			changed.stream().forEach(meta->{
				META_DATA
				.values()
				.stream()
				.filter(md->Objects.equals(md.getId(), meta.getId()))
				.forEach(md->META_DATA.remove(md.getPath()));
				
				META_DATA.put(meta.getPath(), meta);
			});
			break;
		}
		publishConfig(gson, metaDataId, META_DATA);
	}
	@Override
	public void onRuleChanged(List<RuleData> changed, DataEventTypeEnum eventType) {
		Gson gson=new Gson();
		updateRuleMap(gson, getConfig(ruleDataId), null);
		switch (eventType) {
		case DELETE:
			changed.stream().forEach(rule->{
				List<RuleData>ls=RULE_MAP
						.getOrDefault(rule.getSelectorId(), new ArrayList<>())
						.stream()
						.filter(s->!s.getId().equals(rule.getSelectorId()))
						.sorted(ruleCp)
						.collect(Collectors.toList());
				RULE_MAP.put(rule.getSelectorId(), ls);
			});
			break;
		case REFRESH:
		case MYSELF:
			Set<String>set=new HashSet<>(RULE_MAP.keySet());
			changed.stream().forEach(rule->{
				set.remove(rule.getSelectorId());
				List<RuleData>ls=RULE_MAP
						.getOrDefault(rule.getSelectorId(), new ArrayList<>())
						.stream()
						.sorted(ruleCp)
						.collect(Collectors.toList());
				RULE_MAP.put(rule.getSelectorId(), ls);
			});
			RULE_MAP.keySet().removeAll(set);
			break;
		default:
			changed.stream().forEach(rule->{
				List<RuleData>ls=RULE_MAP
						.getOrDefault(rule.getSelectorId(), new ArrayList<>())
						.stream()
						.filter(s->!s.getId().equals(rule.getSelectorId()))
						.sorted(ruleCp)
						.collect(Collectors.toList());
				ls.add(rule);
				RULE_MAP.put(rule.getSelectorId(), ls);
			});
			break;
		}
		
		publishConfig(gson, ruleDataId, RULE_MAP);
	}
	
	private static interface ErrorAction{
		public void act(Object o);
	}
}
