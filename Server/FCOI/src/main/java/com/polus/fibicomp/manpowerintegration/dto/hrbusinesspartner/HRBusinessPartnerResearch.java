package com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
"HRBP_EMAIL",
"HRBP_NAME"
})
public class HRBusinessPartnerResearch {

	@JsonProperty("HRBP_EMAIL")
	private String hRBPEmail;
	@JsonProperty("HRBP_NAME")
	private String hRBPName;

	@JsonProperty("HRBP_EMAIL")
	public String gethRBPEmail() {
		return hRBPEmail;
	}

	@JsonProperty("HRBP_EMAIL")
	public void sethRBPEmail(String hRBPEmail) {
		this.hRBPEmail = hRBPEmail;
	}

	@JsonProperty("HRBP_NAME")
	public String gethRBPName() {
		return hRBPName;
	}

	@JsonProperty("HRBP_NAME")
	public void sethRBPName(String hRBPName) {
		this.hRBPName = hRBPName;
	}

}
