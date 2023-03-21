package com.polus.fibicomp.fastintegration.sapfeedmaintenance.dto;

import java.util.List;

import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;

public class SapFeedMaintenanceDto {

	private List<SapFeedTmplFmBudget> sapFeedTmplFmBudgets;

	private List<SapFeedTmplFundedPrgm> sapFeedTmplFundedPrgms;

	private List<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMasters;

	private List<SapFeedTmplGrantMaster> sapFeedTmplGrantMasters;

	private List<SapFeedTmplProjectDef> sapFeedTmplProjectDefs;

	private List<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgms;

	private List<SapFeedTmplSponsorClass> sapFeedTmplSponsorClasses;

	private List<SapFeedTmplWbs> sapFeedTmplWbs;

	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgets() {
		return sapFeedTmplFmBudgets;
	}

	public void setSapFeedTmplFmBudgets(List<SapFeedTmplFmBudget> sapFeedTmplFmBudgets) {
		this.sapFeedTmplFmBudgets = sapFeedTmplFmBudgets;
	}

	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgms() {
		return sapFeedTmplFundedPrgms;
	}

	public void setSapFeedTmplFundedPrgms(List<SapFeedTmplFundedPrgm> sapFeedTmplFundedPrgms) {
		this.sapFeedTmplFundedPrgms = sapFeedTmplFundedPrgms;
	}

	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasters() {
		return sapFeedTmplGrantBudMasters;
	}

	public void setSapFeedTmplGrantBudMasters(List<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMasters) {
		this.sapFeedTmplGrantBudMasters = sapFeedTmplGrantBudMasters;
	}

	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasters() {
		return sapFeedTmplGrantMasters;
	}

	public void setSapFeedTmplGrantMasters(List<SapFeedTmplGrantMaster> sapFeedTmplGrantMasters) {
		this.sapFeedTmplGrantMasters = sapFeedTmplGrantMasters;
	}

	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefs() {
		return sapFeedTmplProjectDefs;
	}

	public void setSapFeedTmplProjectDefs(List<SapFeedTmplProjectDef> sapFeedTmplProjectDefs) {
		this.sapFeedTmplProjectDefs = sapFeedTmplProjectDefs;
	}

	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgms() {
		return sapFeedTmplSponsoPrgms;
	}

	public void setSapFeedTmplSponsoPrgms(List<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgms) {
		this.sapFeedTmplSponsoPrgms = sapFeedTmplSponsoPrgms;
	}

	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClasses() {
		return sapFeedTmplSponsorClasses;
	}

	public void setSapFeedTmplSponsorClasses(List<SapFeedTmplSponsorClass> sapFeedTmplSponsorClasses) {
		this.sapFeedTmplSponsorClasses = sapFeedTmplSponsorClasses;
	}

	public List<SapFeedTmplWbs> getSapFeedTmplWbs() {
		return sapFeedTmplWbs;
	}

	public void setSapFeedTmplWbs(List<SapFeedTmplWbs> sapFeedTmplWbs) {
		this.sapFeedTmplWbs = sapFeedTmplWbs;
	}

}
