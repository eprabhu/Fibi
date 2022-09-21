package com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
"HRBP_CL_NAME",
"HRBP_CL_EMAIL"
})
public class HRBPClusterLead {

	@JsonProperty("HRBP_CL_NAME")
	private String hRBPClName;
	@JsonProperty("HRBP_CL_EMAIL")
	private String hRBPClEmail;

	@JsonProperty("HRBP_CL_NAME")
	public String gethRBPClName() {
		return hRBPClName;
	}

	@JsonProperty("HRBP_CL_NAME")
	public void sethRBPClName(String hRBPClName) {
		this.hRBPClName = hRBPClName;
	}

	@JsonProperty("HRBP_CL_EMAIL")
	public String gethRBPClEmail() {
		return hRBPClEmail;
	}

	@JsonProperty("HRBP_CL_EMAIL")
	public void sethRBPClEmail(String hRBPClEmail) {
		this.hRBPClEmail = hRBPClEmail;
	}

}
