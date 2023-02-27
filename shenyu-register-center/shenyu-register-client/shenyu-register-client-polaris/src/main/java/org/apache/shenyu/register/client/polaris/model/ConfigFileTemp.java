/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.register.client.polaris.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

/**
 * @author <a href="mailto:liaochuntao@live.com">liaochuntao</a>
 */
public class ConfigFileTemp {

	@JsonProperty("namespace")
	private String namespace;

	@JsonProperty("group")
	private String group;

	@JsonProperty("name")
	private String fileName;

	@JsonProperty("content")
	private String content;

	@JsonProperty("comment")
	private String comment;

	@JsonProperty("tags")
	private List<Tag> tags;

	@JsonProperty("format")
	private String format;

	public String getNamespace() {
		return namespace;
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public List<Tag> getTags() {
		return tags;
	}

	public void setTags(List<Tag> tags) {
		this.tags = tags;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}



	public static class Tag {
		private String key;

		private String value;

		public Tag(String key, String value) {
			this.key = key;
			this.value = value;
		}

		public String getKey() {
			return key;
		}

		public void setKey(String key) {
			this.key = key;
		}

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof ConfigFileTemp)) return false;
		ConfigFileTemp that = (ConfigFileTemp) o;
		return Objects.equals(getNamespace(), that.getNamespace()) && Objects.equals(getGroup(), that.getGroup())
				&& Objects.equals(getFileName(), that.getFileName()) && Objects.equals(getContent(), that.getContent())
				&& Objects.equals(getFormat(), that.getFormat());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getNamespace(), getGroup(), getFileName(), getContent(), getFormat());
	}

	@Override
	public String toString() {
		return "ConfigFileTemp{" +
				"namespace='" + namespace + '\'' +
				", group='" + group + '\'' +
				", fileName='" + fileName + '\'' +
				", content='" + content + '\'' +
				", comment='" + comment + '\'' +
				", tags=" + tags +
				", format='" + format + '\'' +
				'}';
	}

	public static ConfigFileTempBuilder builder() {
		return new ConfigFileTempBuilder();
	}

	public static final class ConfigFileTempBuilder {
		private String namespace;
		private String group;
		private String fileName;
		private String content;
		private String comment;
		private List<Tag> tags;
		private String format;

		private ConfigFileTempBuilder() {
		}


		public ConfigFileTempBuilder namespace(String namespace) {
			this.namespace = namespace;
			return this;
		}

		public ConfigFileTempBuilder group(String group) {
			this.group = group;
			return this;
		}

		public ConfigFileTempBuilder fileName(String fileName) {
			this.fileName = fileName;
			return this;
		}

		public ConfigFileTempBuilder content(String content) {
			this.content = content;
			return this;
		}

		public ConfigFileTempBuilder comment(String comment) {
			this.comment = comment;
			return this;
		}

		public ConfigFileTempBuilder tags(List<Tag> tags) {
			this.tags = tags;
			return this;
		}

		public ConfigFileTempBuilder format(String format) {
			this.format = format;
			return this;
		}

		public ConfigFileTemp build() {
			ConfigFileTemp configFileTemp = new ConfigFileTemp();
			configFileTemp.setNamespace(namespace);
			configFileTemp.setGroup(group);
			configFileTemp.setFileName(fileName);
			configFileTemp.setContent(content);
			configFileTemp.setComment(comment);
			configFileTemp.setTags(tags);
			configFileTemp.setFormat(format);
			return configFileTemp;
		}
	}
}
