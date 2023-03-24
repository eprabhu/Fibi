package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "title",
    "subtitle",
    "translated-title"
})
public class TitleDto {

	@JsonProperty("title")
    private TitleValueDto titleValue;

    @JsonProperty("subtitle")
    private SubtitleDto subtitle;
   
    @JsonProperty("translated-title")
    private TranslatedTitleDto translatedTitle;

    @JsonProperty("title")
	public TitleValueDto getTitleValue() {
		return titleValue;
	}

    @JsonProperty("title")
	public void setTitleValue(TitleValueDto titleValue) {
		this.titleValue = titleValue;
	}

    @JsonProperty("subtitle")
	public SubtitleDto getSubtitle() {
		return subtitle;
	}

    @JsonProperty("subtitle")
	public void setSubtitle(SubtitleDto subtitle) {
		this.subtitle = subtitle;
	}

	@JsonProperty("translated-title")
	public TranslatedTitleDto getTranslatedTitle() {
		return translatedTitle;
	}

	@JsonProperty("translated-title")
	public void setTranslatedTitle(TranslatedTitleDto translatedTitle) {
		this.translatedTitle = translatedTitle;
	}

}
