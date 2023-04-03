package com.polus.fibicomp.coi.vo;

import java.util.List;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.COIEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityRelType;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetails;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetailsStatus;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.person.pojo.Person;

public class ConflictOfInterestVO {

	private CoiDisclosure coiDisclosure;

	private Person person;

	private Integer numberOfSFI;

	private COIFinancialEntity coiFinancialEntity;

	private COIEntity coiEntity;

	private String personId;

	private List<DisclosureDetailDto> proposals;

	private List<DisclosureDetailDto> awards;

	private String searchString;

	private List<EntityStatus> entityStatus;

	private List<EntityType> entityType;

	private List<COIFinancialEntityRelType> coiFinancialEntityRelType;

	private List<CoiDisclosureDetailsStatus> coiDisclosureDetailStatuses;

	private List<COIFinancialEntityDetails> coiFinancialEntityDetails;

	private COIFinancialEntityDetails coiFinancialEntityDetail;

	private List<COIFinancialEntity> coiFinancialEntitys;

	private Integer moduleCode;

	private Integer moduleItemId;

	private List<CoiDisclosureDetails> coiDisclosureDetails;

	private Integer disclosureId;

	private Boolean sfiCompleted;

	private Integer conflictIdentifiedCount;

	private Integer newSubmissionsCount;

	private Integer unassignedCount;

	private Integer pendingEntityApproval;

	private Integer reviewCommentsCount;

	private String reviseComment;
	
	private Integer submoduleCode;

	private String disclosureSequenceStatusCode;

	private List<CoiDisclosure> coiDisclosures;

	private Integer coiFinancialEntityId;
	
    private List<CoiSectionsType> CoiSectionsType;
	
	private CoiReview coiReview;
	
	private List<CoiReview> coiReviews;
	
	private List<AdminGroup> adminGroup;
	
	private List<CoiReviewActivity> coiReviewActivitys;
	
	private List<CoiReviewComments> coiReviewComments;

	private CoiReviewComments coiReviewComment;
	
	private List<CoiReviewCommentAttachment> coiReviewCommentAttachment;

	private String disclosureStatusCode;

	private Integer numberOfProposal;

	private Integer numberOfAward;
	
	private Integer coiSubSectionsId;
	
	private  String coiSectionsTypeCode;
	
	private long commentCount;
	
	private List<Integer> tagGroupId;
	
	private String sort;

	private CoiDisclosureDetails coiDisclosureDetail;

	private String disclosureCategoryType;

	private String proposalIdlinkedInDisclosure;

	private String disclosureNumber;

	private Boolean proposalDisclosureWithNoSfi;
	
	private Integer coiEntityId;
	
	private String entityStatusCode;
	
	private Integer inProgressDisclosureCount;
	
	private Integer approvedDisclosureCount;
	
	private Integer travelDisclosureCount;
	
	private Integer disclosureHistoryCount;
	
	private String filterType;
	
	private List<COIEntity> coiEntityList;

	public CoiDisclosure getCoiDisclosure() {
		return coiDisclosure;
	}

	public void setCoiDisclosure(CoiDisclosure coiDisclosure) {
		this.coiDisclosure = coiDisclosure;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public Integer getNumberOfSFI() {
		return numberOfSFI;
	}

	public void setNumberOfSFI(Integer numberOfSFI) {
		this.numberOfSFI = numberOfSFI;
	}

	public COIFinancialEntity getCoiFinancialEntity() {
		return coiFinancialEntity;
	}

	public void setCoiFinancialEntity(COIFinancialEntity coiFinancialEntity) {
		this.coiFinancialEntity = coiFinancialEntity;
	}

	public COIEntity getCoiEntity() {
		return coiEntity;
	}

	public void setCoiEntity(COIEntity coiEntity) {
		this.coiEntity = coiEntity;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<DisclosureDetailDto> getProposals() {
		return proposals;
	}

	public void setProposals(List<DisclosureDetailDto> proposals) {
		this.proposals = proposals;
	}

	public List<DisclosureDetailDto> getAwards() {
		return awards;
	}

	public void setAwards(List<DisclosureDetailDto> awards) {
		this.awards = awards;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public List<EntityType> getEntityType() {
		return entityType;
	}

	public void setEntityType(List<EntityType> entityType) {
		this.entityType = entityType;
	}

	public List<EntityStatus> getEntityStatus() {
		return entityStatus;
	}

	public void setEntityStatus(List<EntityStatus> entityStatus) {
		this.entityStatus = entityStatus;
	}

	public List<COIFinancialEntityRelType> getCoiFinancialEntityRelType() {
		return coiFinancialEntityRelType;
	}

	public void setCoiFinancialEntityRelType(List<COIFinancialEntityRelType> coiFinancialEntityRelType) {
		this.coiFinancialEntityRelType = coiFinancialEntityRelType;
	}

	public List<CoiDisclosureDetailsStatus> getCoiDisclosureDetailStatuses() {
		return coiDisclosureDetailStatuses;
	}

	public void setCoiDisclosureDetailStatuses(List<CoiDisclosureDetailsStatus> coiDisclosureDetailStatuses) {
		this.coiDisclosureDetailStatuses = coiDisclosureDetailStatuses;
	}
	public List<COIFinancialEntityDetails> getCoiFinancialEntityDetails() {
		return coiFinancialEntityDetails;
	}

	public void setCoiFinancialEntityDetails(List<COIFinancialEntityDetails> coiFinancialEntityDetails) {
		this.coiFinancialEntityDetails = coiFinancialEntityDetails;
	}

	public COIFinancialEntityDetails getCoiFinancialEntityDetail() {
		return coiFinancialEntityDetail;
	}

	public void setCoiFinancialEntityDetail(COIFinancialEntityDetails coiFinancialEntityDetail) {
		this.coiFinancialEntityDetail = coiFinancialEntityDetail;
	}

	public List<COIFinancialEntity> getCoiFinancialEntitys() {
		return coiFinancialEntitys;
	}

	public void setCoiFinancialEntitys(List<COIFinancialEntity> coiFinancialEntitys) {
		this.coiFinancialEntitys = coiFinancialEntitys;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(Integer moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public List<CoiDisclosureDetails> getCoiDisclosureDetails() {
		return coiDisclosureDetails;
	}

	public void setCoiDisclosureDetails(List<CoiDisclosureDetails> coiDisclosureDetails) {
		this.coiDisclosureDetails = coiDisclosureDetails;
	}

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public Boolean getSfiCompleted() {
		return sfiCompleted;
	}

	public void setSfiCompleted(Boolean sfiCompleted) {
		this.sfiCompleted = sfiCompleted;
	}

	public Integer getConflictIdentifiedCount() {
		return conflictIdentifiedCount;
	}

	public void setConflictIdentifiedCount(Integer conflictIdentifiedCount) {
		this.conflictIdentifiedCount = conflictIdentifiedCount;
	}

	public Integer getNewSubmissionsCount() {
		return newSubmissionsCount;
	}

	public void setNewSubmissionsCount(Integer newSubmissionsCount) {
		this.newSubmissionsCount = newSubmissionsCount;
	}

	public Integer getUnassignedCount() {
		return unassignedCount;
	}

	public void setUnassignedCount(Integer unassignedCount) {
		this.unassignedCount = unassignedCount;
	}

	public Integer getPendingEntityApproval() {
		return pendingEntityApproval;
	}

	public void setPendingEntityApproval(Integer pendingEntityApproval) {
		this.pendingEntityApproval = pendingEntityApproval;
	}

	public Integer getReviewCommentsCount() {
		return reviewCommentsCount;
	}

	public void setReviewCommentsCount(Integer reviewCommentsCount) {
		this.reviewCommentsCount = reviewCommentsCount;
	}

	public String getReviseComment() {
		return reviseComment;
	}

	public void setReviseComment(String reviseComment) {
		this.reviseComment = reviseComment;
	}

	public Integer getSubmoduleCode() {
		return submoduleCode;
	}

	public void setSubmoduleCode(Integer submoduleCode) {
		this.submoduleCode = submoduleCode;
	}

	public String getDisclosureSequenceStatusCode() {
		return disclosureSequenceStatusCode;
	}

	public void setDisclosureSequenceStatusCode(String disclosureSequenceStatusCode) {
		this.disclosureSequenceStatusCode = disclosureSequenceStatusCode;
	}

	public List<CoiDisclosure> getCoiDisclosures() {
		return coiDisclosures;
	}

	public void setCoiDisclosures(List<CoiDisclosure> coiDisclosures) {
		this.coiDisclosures = coiDisclosures;
	}

	public Integer getCoiFinancialEntityId() {
		return coiFinancialEntityId;
	}

	public void setCoiFinancialEntityId(Integer coiFinancialEntityId) {
		this.coiFinancialEntityId = coiFinancialEntityId;
	}

	public List<CoiSectionsType> getCoiSectionsType() {
		return CoiSectionsType;
	}

	public void setCoiSectionsType(List<CoiSectionsType> coiSectionsType) {
		CoiSectionsType = coiSectionsType;
	}

	public CoiReview getCoiReview() {
		return coiReview;
	}

	public void setCoiReview(CoiReview coiReview) {
		this.coiReview = coiReview;
	}

	public List<CoiReview> getCoiReviews() {
		return coiReviews;
	}

	public void setCoiReviews(List<CoiReview> coiReviews) {
		this.coiReviews = coiReviews;
	}

	public List<AdminGroup> getAdminGroup() {
		return adminGroup;
	}

	public void setAdminGroup(List<AdminGroup> adminGroup) {
		this.adminGroup = adminGroup;
	}

	public List<CoiReviewActivity> getCoiReviewActivitys() {
		return coiReviewActivitys;
	}

	public void setCoiReviewActivitys(List<CoiReviewActivity> coiReviewActivitys) {
		this.coiReviewActivitys = coiReviewActivitys;
	}

	public List<CoiReviewComments> getCoiReviewComments() {
		return coiReviewComments;
	}

	public void setCoiReviewComments(List<CoiReviewComments> coiReviewComments) {
		this.coiReviewComments = coiReviewComments;
	}

	public CoiReviewComments getCoiReviewComment() {
		return coiReviewComment;
	}

	public void setCoiReviewComment(CoiReviewComments coiReviewComment) {
		this.coiReviewComment = coiReviewComment;
	}

	public List<CoiReviewCommentAttachment> getCoiReviewCommentAttachment() {
		return coiReviewCommentAttachment;
	}

	public void setCoiReviewCommentAttachment(List<CoiReviewCommentAttachment> coiReviewCommentAttachment) {
		this.coiReviewCommentAttachment = coiReviewCommentAttachment;
	}

	public String getDisclosureStatusCode() {
		return disclosureStatusCode;
	}

	public void setDisclosureStatusCode(String disclosureStatusCode) {
		this.disclosureStatusCode = disclosureStatusCode;
	}

	public Integer getNumberOfProposal() {
		return numberOfProposal;
	}

	public void setNumberOfProposal(Integer numberOfProposal) {
		this.numberOfProposal = numberOfProposal;
	}

	public Integer getNumberOfAward() {
		return numberOfAward;
	}

	public void setNumberOfAward(Integer numberOfAward) {
		this.numberOfAward = numberOfAward;
	}

	public Integer getCoiSubSectionsId() {
		return coiSubSectionsId;
	}

	public void setCoiSubSectionsId(Integer coiSubSectionsId) {
		this.coiSubSectionsId = coiSubSectionsId;
	}

	public String getCoiSectionsTypeCode() {
		return coiSectionsTypeCode;
	}

	public void setCoiSectionsTypeCode(String coiSectionsTypeCode) {
		this.coiSectionsTypeCode = coiSectionsTypeCode;
	}

	public long getCommentCount() {
		return commentCount;
	}

	public void setCommentCount(long commentCount) {
		this.commentCount = commentCount;
	}

	public List<Integer> getTagGroupId() {
		return tagGroupId;
	}

	public void setTagGroupId(List<Integer> tagGroupId) {
		this.tagGroupId = tagGroupId;
	}

	public String getSort() {
		return sort;
	}

	public void setSort(String sort) {
		this.sort = sort;
	}

	public CoiDisclosureDetails getCoiDisclosureDetail() {
		return coiDisclosureDetail;
	}

	public void setCoiDisclosureDetail(CoiDisclosureDetails coiDisclosureDetail) {
		this.coiDisclosureDetail = coiDisclosureDetail;
	}

	public String getDisclosureCategoryType() {
		return disclosureCategoryType;
	}

	public void setDisclosureCategoryType(String disclosureCategoryType) {
		this.disclosureCategoryType = disclosureCategoryType;
	}

	public String getProposalIdlinkedInDisclosure() {
		return proposalIdlinkedInDisclosure;
	}

	public void setProposalIdlinkedInDisclosure(String proposalIdlinkedInDisclosure) {
		this.proposalIdlinkedInDisclosure = proposalIdlinkedInDisclosure;
	}

	public String getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(String disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
	}

	public Boolean getProposalDisclosureWithNoSfi() {
		return proposalDisclosureWithNoSfi;
	}

	public void setProposalDisclosureWithNoSfi(Boolean proposalDisclosureWithNoSfi) {
		this.proposalDisclosureWithNoSfi = proposalDisclosureWithNoSfi;
	}

	public Integer getCoiEntityId() {
		return coiEntityId;
	}

	public void setCoiEntityId(Integer coiEntityId) {
		this.coiEntityId = coiEntityId;
	}

	public String getEntityStatusCode() {
		return entityStatusCode;
	}

	public void setEntityStatusCode(String entityStatusCode) {
		this.entityStatusCode = entityStatusCode;
	}

	public Integer getInProgressDisclosureCount() {
		return inProgressDisclosureCount;
	}

	public void setInProgressDisclosureCount(Integer inProgressDisclosureCount) {
		this.inProgressDisclosureCount = inProgressDisclosureCount;
	}

	public Integer getApprovedDisclosureCount() {
		return approvedDisclosureCount;
	}

	public void setApprovedDisclosureCount(Integer approvedDisclosureCount) {
		this.approvedDisclosureCount = approvedDisclosureCount;
	}

	public Integer getTravelDisclosureCount() {
		return travelDisclosureCount;
	}

	public void setTravelDisclosureCount(Integer travelDisclosureCount) {
		this.travelDisclosureCount = travelDisclosureCount;
	}

	public Integer getDisclosureHistoryCount() {
		return disclosureHistoryCount;
	}

	public void setDisclosureHistoryCount(Integer disclosureHistoryCount) {
		this.disclosureHistoryCount = disclosureHistoryCount;
	}

	public String getFilterType() {
		return filterType;
	}

	public void setFilterType(String filterType) {
		this.filterType = filterType;
	}

	public List<COIEntity> getCoiEntityList() {
		return coiEntityList;
	}

	public void setCoiEntityList(List<COIEntity> coiEntityList) {
		this.coiEntityList = coiEntityList;
	}

}
