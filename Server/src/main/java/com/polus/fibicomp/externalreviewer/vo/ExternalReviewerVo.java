package com.polus.fibicomp.externalreviewer.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicArea;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicRank;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAffilation;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachment;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerCira;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerOriginality;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerThoroughness;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewer;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerAttachmentType;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerExt;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerSpecialization;
import com.polus.fibicomp.externalreviewer.pojo.ReviewerRights;
import com.polus.fibicomp.pojo.Country;

public class ExternalReviewerVo {
	
	private ExternalReviewer extReviewer;
	
	private ExternalReviewerExt externalReviewerExt;
	
	private ExternalReviewerRights externalReviewerRight;

    private Integer extReviewerId;
	
	private String message;
	
	private Integer currentPage;
	
	private Integer pageNumber;
	
	private Integer totalExtReviewer;
	
	private String property1;
	
	private String property2;

	private String property3;

	private String property4;

	private String property5;
	
	private String property6;
	
	private String property7;
	
	private String property8;
	
	private String property9;
	
	private List<String> property10;
	
	private List<String> property11;
	
	private String property12;
	
	private List<ExternalReviewer> extReviewers;
	
	private List<ExternalReviewerSpecialization> externalReviewerSpecializations;
	
	private List<ExternalReviewerAttachmentType> externalReviewerAttachmentType;
	
	private List<ExtReviewerAcademicArea> extReviewerAcademicArea;
	
	private List<ExtReviewerAcademicRank> extReviewerAcademicRank;
	
	private List<ExtReviewerAffilation> extReviewerAffilation;
	
	private List<ExtReviewerCira> extReviewerCira;
	
	private List<ExtReviewerOriginality> extReviewerOriginality;
	
	private List<ReviewerRights> reviewerRights;
	
	private List<Country> country;
	
	private List<ExtReviewerThoroughness> extReviewerThoroughness;
	
	private List<ExtReviewerAttachment> extReviewerAttachments;
	
	private ExtReviewerAttachment extReviewerAttachment;
	
	private String sortBy;
	
	private String reverse;
	
	private Map<String, String> sort = new HashMap<>();
	
	private Integer extReviewerAttachmentId;
	
	private String searchString;

	public ExternalReviewer getExtReviewer() {
		return extReviewer;
	}

	public void setExtReviewer(ExternalReviewer extReviewer) {
		this.extReviewer = extReviewer;
	}

	public Integer getExtReviewerId() {
		return extReviewerId;
	}

	public void setExtReviewerId(Integer extReviewerId) {
		this.extReviewerId = extReviewerId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public Integer getTotalExtReviewer() {
		return totalExtReviewer;
	}

	public void setTotalExtReviewer(Integer totalExtReviewer) {
		this.totalExtReviewer = totalExtReviewer;
	}

	public String getProperty1() {
		return property1;
	}

	public void setProperty1(String property1) {
		this.property1 = property1;
	}

	public String getProperty2() {
		return property2;
	}

	public void setProperty2(String property2) {
		this.property2 = property2;
	}

	public String getProperty3() {
		return property3;
	}

	public void setProperty3(String property3) {
		this.property3 = property3;
	}

	public String getProperty4() {
		return property4;
	}

	public void setProperty4(String property4) {
		this.property4 = property4;
	}

	public String getProperty6() {
		return property6;
	}

	public void setProperty6(String property6) {
		this.property6 = property6;
	}

	public List<ExternalReviewer> getExtReviewers() {
		return extReviewers;
	}

	public void setExtReviewers(List<ExternalReviewer> extReviewers) {
		this.extReviewers = extReviewers;
	}

	public ExternalReviewerExt getExternalReviewerExt() {
		return externalReviewerExt;
	}

	public void setExternalReviewerExt(ExternalReviewerExt externalReviewerExt) {
		this.externalReviewerExt = externalReviewerExt;
	}

	public ExternalReviewerRights getExternalReviewerRight() {
		return externalReviewerRight;
	}

	public void setExternalReviewerRight(ExternalReviewerRights externalReviewerRight) {
		this.externalReviewerRight = externalReviewerRight;
	}

	public List<ExternalReviewerSpecialization> getExternalReviewerSpecializations() {
		return externalReviewerSpecializations;
	}

	public void setExternalReviewerSpecializations(List<ExternalReviewerSpecialization> externalReviewerSpecializations) {
		this.externalReviewerSpecializations = externalReviewerSpecializations;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public List<String> getProperty10() {
		return property10;
	}

	public void setProperty10(List<String> property10) {
		this.property10 = property10;
	}

	public List<String> getProperty11() {
		return property11;
	}

	public void setProperty11(List<String> property11) {
		this.property11 = property11;
	}

	public String getProperty12() {
		return property12;
	}

	public void setProperty12(String property12) {
		this.property12 = property12;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public List<ExternalReviewerAttachmentType> getExternalReviewerAttachmentType() {
		return externalReviewerAttachmentType;
	}

	public void setExternalReviewerAttachmentType(List<ExternalReviewerAttachmentType> externalReviewerAttachmentType) {
		this.externalReviewerAttachmentType = externalReviewerAttachmentType;
	}

	public List<ExtReviewerAcademicArea> getExtReviewerAcademicArea() {
		return extReviewerAcademicArea;
	}

	public void setExtReviewerAcademicArea(List<ExtReviewerAcademicArea> extReviewerAcademicArea) {
		this.extReviewerAcademicArea = extReviewerAcademicArea;
	}

	public List<ExtReviewerAcademicRank> getExtReviewerAcademicRank() {
		return extReviewerAcademicRank;
	}

	public void setExtReviewerAcademicRank(List<ExtReviewerAcademicRank> extReviewerAcademicRank) {
		this.extReviewerAcademicRank = extReviewerAcademicRank;
	}

	public List<ExtReviewerAffilation> getExtReviewerAffilation() {
		return extReviewerAffilation;
	}

	public void setExtReviewerAffilation(List<ExtReviewerAffilation> extReviewerAffilation) {
		this.extReviewerAffilation = extReviewerAffilation;
	}

	public List<ExtReviewerCira> getExtReviewerCira() {
		return extReviewerCira;
	}

	public void setExtReviewerCira(List<ExtReviewerCira> extReviewerCira) {
		this.extReviewerCira = extReviewerCira;
	}

	public List<ExtReviewerOriginality> getExtReviewerOriginality() {
		return extReviewerOriginality;
	}

	public void setExtReviewerOriginality(List<ExtReviewerOriginality> extReviewerOriginality) {
		this.extReviewerOriginality = extReviewerOriginality;
	}

	public List<ReviewerRights> getReviewerRights() {
		return reviewerRights;
	}

	public void setReviewerRights(List<ReviewerRights> reviewerRights) {
		this.reviewerRights = reviewerRights;
	}

	public List<ExtReviewerThoroughness> getExtReviewerThoroughness() {
		return extReviewerThoroughness;
	}

	public void setExtReviewerThoroughness(List<ExtReviewerThoroughness> extReviewerThoroughness) {
		this.extReviewerThoroughness = extReviewerThoroughness;
	}

	public List<Country> getCountry() {
		return country;
	}

	public void setCountry(List<Country> country) {
		this.country = country;
	}

	public List<ExtReviewerAttachment> getExtReviewerAttachments() {
		return extReviewerAttachments;
	}

	public void setExtReviewerAttachments(List<ExtReviewerAttachment> extReviewerAttachments) {
		this.extReviewerAttachments = extReviewerAttachments;
	}

	public Integer getExtReviewerAttachmentId() {
		return extReviewerAttachmentId;
	}

	public void setExtReviewerAttachmentId(Integer extReviewerAttachmentId) {
		this.extReviewerAttachmentId = extReviewerAttachmentId;
	}

	public ExtReviewerAttachment getExtReviewerAttachment() {
		return extReviewerAttachment;
	}

	public void setExtReviewerAttachment(ExtReviewerAttachment extReviewerAttachment) {
		this.extReviewerAttachment = extReviewerAttachment;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

}
