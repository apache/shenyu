package org.apache.shenyu.admin.discovery;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.common.utils.GsonUtils;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

/**
 * DiscoveryUpstreamParser.
 * <p>
 * You can define a custom map mapper if your custom upstream doesn't fit
 * <p/>
 */
public class DiscoveryUpstreamParser implements JsonDeserializer<DiscoveryUpstreamDTO> {

    private final Map<String, String> conversion;

    public DiscoveryUpstreamParser(final Map<String, String> conversion) {
        this.conversion = conversion;
    }

    @Override
    public DiscoveryUpstreamDTO deserialize(final JsonElement jsonElement,
                                            final Type type,
                                            final JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject asJsonObject = jsonElement.getAsJsonObject();
        JsonObject afterJson = new JsonObject();
        for (Map.Entry<String, JsonElement> elementEntry : asJsonObject.entrySet()) {
            String key = elementEntry.getKey();
            if (conversion.containsKey(key)) {
                String transferKey = conversion.get(key);
                afterJson.add(transferKey, elementEntry.getValue());
            } else {
                afterJson.add(key, elementEntry.getValue());
            }
        }
        return GsonUtils.getInstance().fromJson(afterJson, DiscoveryUpstreamDTO.class);
    }

    public DiscoveryUpstreamDTO parse(final String jsonString) {
        GsonBuilder gsonBuilder = new GsonBuilder().registerTypeAdapter(DiscoveryUpstreamDTO.class, this);
        Gson gson = gsonBuilder.create();
        return gson.fromJson(jsonString, DiscoveryUpstreamDTO.class);
    }

    public List<DiscoveryUpstreamDTO> parseList(final String jsonString) {
        GsonBuilder gsonBuilder = new GsonBuilder().registerTypeAdapter(DiscoveryUpstreamDTO.class, this);
        Gson gson = gsonBuilder.create();
        return gson.fromJson(jsonString, new TypeToken<List<DiscoveryUpstreamDTO>>(){}.getType());
    }

}
