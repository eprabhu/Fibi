package com.polus.fibicomp.orcid.dto;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "last-modified-date",
    "group",
    "path"
})
public class WorksDto {

	@JsonProperty("last-modified-date")
	private LastModifiedDateDto lastModifiedDate;

	@JsonProperty("group")
	private List<GroupDto> group = null;

	@JsonProperty("path")
	private String path;

	@JsonProperty("last-modified-date")
	public LastModifiedDateDto getLastModifiedDate() {
		return lastModifiedDate;
	}

	@JsonProperty("last-modified-date")
	public void setLastModifiedDate(LastModifiedDateDto lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
	}

	@JsonProperty("group")
	public List<GroupDto> getGroup() {
		return group;
	}

	@JsonProperty("group")
	public void setGroup(List<GroupDto> group) {
		this.group = group;
	}

	@JsonProperty("path")
	public String getPath() {
		return path;
	}

	@JsonProperty("path")
	public void setPath(String path) {
		this.path = path;
	}

}
