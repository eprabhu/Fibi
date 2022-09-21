package com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
"Supervisory_Organization_ID",
"HRBP_Cluster_Lead",
"HR_Business_Partner_Research"
})
public class HRBPReportEntry {

	@JsonProperty("Supervisory_Organization_ID")
	private String supervisoryOrganizationID;
	@JsonProperty("HRBP_Cluster_Lead")
	private List<HRBPClusterLead> hRBPClusterLeads = null;
	@JsonProperty("HR_Business_Partner_Research")
	private List<HRBusinessPartnerResearch> hRBusinessPartnerResearchers = null;

	@JsonProperty("Supervisory_Organization_ID")
	public String getSupervisoryOrganizationID() {
		return supervisoryOrganizationID;
	}

	@JsonProperty("Supervisory_Organization_ID")
	public void setSupervisoryOrganizationID(String supervisoryOrganizationID) {
		this.supervisoryOrganizationID = supervisoryOrganizationID;
	}

	@JsonProperty("HRBP_Cluster_Lead")
	public List<HRBPClusterLead> gethRBPClusterLeads() {
		return hRBPClusterLeads;
	}

	@JsonProperty("HRBP_Cluster_Lead")
	public void sethRBPClusterLeads(List<HRBPClusterLead> hRBPClusterLeads) {
		this.hRBPClusterLeads = hRBPClusterLeads;
	}

	@JsonProperty("HR_Business_Partner_Research")
	public List<HRBusinessPartnerResearch> gethRBusinessPartnerResearchers() {
		return hRBusinessPartnerResearchers;
	}

	@JsonProperty("HR_Business_Partner_Research")
	public void sethRBusinessPartnerResearchers(List<HRBusinessPartnerResearch> hRBusinessPartnerResearchers) {
		this.hRBusinessPartnerResearchers = hRBusinessPartnerResearchers;
	}

}
