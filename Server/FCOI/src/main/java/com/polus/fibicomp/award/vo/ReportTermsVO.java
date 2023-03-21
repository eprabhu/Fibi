package com.polus.fibicomp.award.vo;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.Distribution;
import com.polus.fibicomp.award.pojo.Frequency;
import com.polus.fibicomp.award.pojo.FrequencyBase;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.pojo.ReportClass;
import com.polus.fibicomp.award.pojo.ReportStatus;
import com.polus.fibicomp.award.pojo.SponsorTerm;
import com.polus.fibicomp.award.pojo.SponsorTermType;

public class ReportTermsVO {

	private Integer awardId;
	
	private AwardSponsorTerm awardSponsorTerm;
	
	private AwardReportTerms awardReport;
	
	private AwardApprovedEquipment awardApprovedEquipment;
	
	private AwardAprovedForeignTravel awardAprovedForeignTravel;

	private AwardReportTracking awardReportTracking;
	
	private AwardReportTrackingFile awardReportTrackingFile;
  
	private List<ReportClass> reportClassList;
	 
	private List<Report> reportList;
	
	private List<Distribution> distributionList;
	
	private List<Frequency> frequencyList;
	
	private List<FrequencyBase> frequencyBaseList;

	private List<SponsorTermType> sponsorTermTypeList;
	
	private List<SponsorTerm> sponsorTermList;
	
	private List<AwardSponsorTerm> awardSponsorTermsList;
	
	private List<AwardApprovedEquipment> awardApprovedEquipmentList;
	
	private List<AwardAprovedForeignTravel> awardAprovedForeignTravelList;
	
	private List<AwardPerson> awardForeignTravellerList;
	
	private List<AwardReportTerms> awardReportTerms;
	
	private Map<String, List<HashMap<String, Object>>> awardTermsList;
	
	private Map<String, List<AwardReportTerms>> awardReportsList;
	
	private AwardReportTermRecipient awardReportTermRecipient;
	
	private String acType;
	
	private String message;

	private Integer awardReportTermsId;

	private List<AwardReportTracking> awardReportTrackingList;

	private List<ReportStatus> reportStatusList;

	private Map<String, Timestamp> mapOfDates;

	private Boolean frequenciesChanged = false;

	private Boolean isProgressReportEnabled = false;

	private Boolean isReplaceAttachmentEnabled = Boolean.FALSE;
	
	private Boolean isEditEnabledForSection = Boolean.TRUE;

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<ReportClass> getReportClassList() {
		return reportClassList;
	}

	public void setReportClassList(List<ReportClass> reportClassList) {
		this.reportClassList = reportClassList;
	}

	public List<Report> getReportList() {
		return reportList;
	}

	public void setReportList(List<Report> reportList) {
		this.reportList = reportList;
	}

	public List<Distribution> getDistributionList() {
		return distributionList;
	}

	public void setDistributionList(List<Distribution> distributionList) {
		this.distributionList = distributionList;
	}

	public List<Frequency> getFrequencyList() {
		return frequencyList;
	}

	public void setFrequencyList(List<Frequency> frequencyList) {
		this.frequencyList = frequencyList;
	}

	public List<FrequencyBase> getFrequencyBaseList() {
		return frequencyBaseList;
	}

	public void setFrequencyBaseList(List<FrequencyBase> frequencyBaseList) {
		this.frequencyBaseList = frequencyBaseList;
	}

	public List<SponsorTermType> getSponsorTermTypeList() {
		return sponsorTermTypeList;
	}

	public void setSponsorTermTypeList(List<SponsorTermType> sponsorTermTypeList) {
		this.sponsorTermTypeList = sponsorTermTypeList;
	}

	public AwardSponsorTerm getAwardSponsorTerm() {
		return awardSponsorTerm;
	}

	public void setAwardSponsorTerm(AwardSponsorTerm awardSponsorTerm) {
		this.awardSponsorTerm = awardSponsorTerm;
	}

	public AwardReportTerms getAwardReport() {
		return awardReport;
	}

	public void setAwardReport(AwardReportTerms awardReport) {
		this.awardReport = awardReport;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public List<SponsorTerm> getSponsorTermList() {
		return sponsorTermList;
	}

	public void setSponsorTermList(List<SponsorTerm> sponsorTermList) {
		this.sponsorTermList = sponsorTermList;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<AwardSponsorTerm> getAwardSponsorTermsList() {
		return awardSponsorTermsList;
	}

	public void setAwardSponsorTermsList(List<AwardSponsorTerm> awardSponsorTermsList) {
		this.awardSponsorTermsList = awardSponsorTermsList;
	}

	public AwardApprovedEquipment getAwardApprovedEquipment() {
		return awardApprovedEquipment;
	}

	public void setAwardApprovedEquipment(AwardApprovedEquipment awardApprovedEquipment) {
		this.awardApprovedEquipment = awardApprovedEquipment;
	}

	public List<AwardApprovedEquipment> getAwardApprovedEquipmentList() {
		return awardApprovedEquipmentList;
	}

	public void setAwardApprovedEquipmentList(List<AwardApprovedEquipment> awardApprovedEquipmentList) {
		this.awardApprovedEquipmentList = awardApprovedEquipmentList;
	}

	public AwardAprovedForeignTravel getAwardAprovedForeignTravel() {
		return awardAprovedForeignTravel;
	}

	public void setAwardAprovedForeignTravel(AwardAprovedForeignTravel awardAprovedForeignTravel) {
		this.awardAprovedForeignTravel = awardAprovedForeignTravel;
	}

	public List<AwardAprovedForeignTravel> getAwardAprovedForeignTravelList() {
		return awardAprovedForeignTravelList;
	}

	public void setAwardAprovedForeignTravelList(List<AwardAprovedForeignTravel> awardAprovedForeignTravelList) {
		this.awardAprovedForeignTravelList = awardAprovedForeignTravelList;
	}

	public List<AwardPerson> getAwardForeignTravellerList() {
		return awardForeignTravellerList;
	}

	public void setAwardForeignTravellerList(List<AwardPerson> awardForeignTravellerList) {
		this.awardForeignTravellerList = awardForeignTravellerList;
	}

	public Map<String, List<HashMap<String, Object>>> getAwardTermsList() {
		return awardTermsList;
	}

	public void setAwardTermsList(Map<String, List<HashMap<String, Object>>> awardTermsList) {
		this.awardTermsList = awardTermsList;
	}

	public AwardReportTermRecipient getAwardReportTermRecipient() {
		return awardReportTermRecipient;
	}

	public void setAwardReportTermRecipient(AwardReportTermRecipient awardReportTermRecipient) {
		this.awardReportTermRecipient = awardReportTermRecipient;
	}

	public List<AwardReportTerms> getAwardReportTerms() {
		return awardReportTerms;
	}

	public void setAwardReportTerms(List<AwardReportTerms> awardReportTerms) {
		this.awardReportTerms = awardReportTerms;
	}

	public AwardReportTracking getAwardReportTracking() {
		return awardReportTracking;
	}

	public void setAwardReportTracking(AwardReportTracking awardReportTracking) {
		this.awardReportTracking = awardReportTracking;
	}

	public AwardReportTrackingFile getAwardReportTrackingFile() {
		return awardReportTrackingFile;
	}

	public void setAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile) {
		this.awardReportTrackingFile = awardReportTrackingFile;
	}

	public Integer getAwardReportTermsId() {
		return awardReportTermsId;
	}

	public void setAwardReportTermsId(Integer awardReportTermsId) {
		this.awardReportTermsId = awardReportTermsId;
	}

	public List<AwardReportTracking> getAwardReportTrackingList() {
		return awardReportTrackingList;
	}

	public void setAwardReportTrackingList(List<AwardReportTracking> awardReportTrackingList) {
		this.awardReportTrackingList = awardReportTrackingList;
	}

	public List<ReportStatus> getReportStatusList() {
		return reportStatusList;
	}

	public void setReportStatusList(List<ReportStatus> reportStatusList) {
		this.reportStatusList = reportStatusList;
	}

	public Map<String, Timestamp> getMapOfDates() {
		return mapOfDates;
	}

	public void setMapOfDates(Map<String, Timestamp> mapOfDates) {
		this.mapOfDates = mapOfDates;
	}

	public Boolean getFrequenciesChanged() {
		return frequenciesChanged;
	}

	public void setFrequenciesChanged(Boolean frequenciesChanged) {
		this.frequenciesChanged = frequenciesChanged;
	}

	public Map<String, List<AwardReportTerms>> getAwardReportsList() {
		return awardReportsList;
	}

	public void setAwardReportsList(Map<String, List<AwardReportTerms>> awardReportsList) {
		this.awardReportsList = awardReportsList;
	}

	public Boolean getProgressReportEnabled() {
		return isProgressReportEnabled;
	}

	public void setProgressReportEnabled(Boolean progressReportEnabled) {
		isProgressReportEnabled = progressReportEnabled;
	}

	public Boolean getIsReplaceAttachmentEnabled() {
		return isReplaceAttachmentEnabled;
	}

	public void setIsReplaceAttachmentEnabled(Boolean isReplaceAttachmentEnabled) {
		this.isReplaceAttachmentEnabled = isReplaceAttachmentEnabled;
	}

	public Boolean getIsEditEnabledForSection() {
		return isEditEnabledForSection;
	}

	public void setIsEditEnabledForSection(Boolean isEditEnabledForSection) {
		this.isEditEnabledForSection = isEditEnabledForSection;
	}
}
