package com.polus.fibicomp.prereview.vo;

import java.util.List;

import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewComment;
import com.polus.fibicomp.prereview.pojo.PreReviewSectionType;
import com.polus.fibicomp.prereview.pojo.PreReviewType;
import com.polus.fibicomp.vo.BaseVO;

public class PreReviewVO extends BaseVO {

	private PreReview newPreReview;

	private String personId;

	private Boolean preReviewExist = false;	

	private List<PreReview> preReviews;

	private PreReview reviewerReview;

	private Boolean isPreReviewer = false;

	private Integer moduleItemCode;

	private Integer moduleSubItemCode;

	private String moduleItemKey;

	private String moduleSubItemKey;

	private String reviewTypeCode;

	private List<PreReviewType> preReviewTypes;

	private List<PreReviewSectionType> preReviewSectionTypes;

	private PreReviewComment preNewReviewComment;

	private Integer preReviewId;

	private String userName;

	private String sortBy;

	private String reverse;

	private String actionType;

	private Integer notificationTypeId;

	private String reviewSectionTypeCode;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String fileName;

	private String contentType;

	private String fileTimestamp;

	private String loginPersonUnitNumber;

	private Integer limit;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getPreReviewExist() {
		return preReviewExist;
	}

	public void setPreReviewExist(Boolean preReviewExist) {
		this.preReviewExist = preReviewExist;
	}

	public PreReview getReviewerReview() {
		return reviewerReview;
	}

	public void setReviewerReview(PreReview reviewerReview) {
		this.reviewerReview = reviewerReview;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public Integer getModuleSubItemCode() {
		return moduleSubItemCode;
	}

	public void setModuleSubItemCode(Integer moduleSubItemCode) {
		this.moduleSubItemCode = moduleSubItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleSubItemKey() {
		return moduleSubItemKey;
	}

	public void setModuleSubItemKey(String moduleSubItemKey) {
		this.moduleSubItemKey = moduleSubItemKey;
	}

	public String getReviewTypeCode() {
		return reviewTypeCode;
	}

	public void setReviewTypeCode(String reviewTypeCode) {
		this.reviewTypeCode = reviewTypeCode;
	}

	public List<PreReview> getPreReviews() {
		return preReviews;
	}

	public void setPreReviews(List<PreReview> preReviews) {
		this.preReviews = preReviews;
	}

	public PreReview getNewPreReview() {
		return newPreReview;
	}

	public void setNewPreReview(PreReview newPreReview) {
		this.newPreReview = newPreReview;
	}

	public List<PreReviewType> getPreReviewTypes() {
		return preReviewTypes;
	}

	public void setPreReviewTypes(List<PreReviewType> preReviewTypes) {
		this.preReviewTypes = preReviewTypes;
	}

	public List<PreReviewSectionType> getPreReviewSectionTypes() {
		return preReviewSectionTypes;
	}

	public void setPreReviewSectionTypes(List<PreReviewSectionType> preReviewSectionTypes) {
		this.preReviewSectionTypes = preReviewSectionTypes;
	}

	public PreReviewComment getPreNewReviewComment() {
		return preNewReviewComment;
	}

	public void setPreNewReviewComment(PreReviewComment preNewReviewComment) {
		this.preNewReviewComment = preNewReviewComment;
	}

	public Integer getPreReviewId() {
		return preReviewId;
	}

	public void setPreReviewId(Integer preReviewId) {
		this.preReviewId = preReviewId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
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

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public Boolean getIsPreReviewer() {
		return isPreReviewer;
	}

	public void setIsPreReviewer(Boolean isPreReviewer) {
		this.isPreReviewer = isPreReviewer;
	}

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public String getReviewSectionTypeCode() {
		return reviewSectionTypeCode;
	}

	public void setReviewSectionTypeCode(String reviewSectionTypeCode) {
		this.reviewSectionTypeCode = reviewSectionTypeCode;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getLoginPersonUnitNumber() {
		return loginPersonUnitNumber;
	}

	public void setLoginPersonUnitNumber(String loginPersonUnitNumber) {
		this.loginPersonUnitNumber = loginPersonUnitNumber;
	}

	public Integer getLimit() {
		return limit;
	}

	public void setLimit(Integer limit) {
		this.limit = limit;
	}

}
