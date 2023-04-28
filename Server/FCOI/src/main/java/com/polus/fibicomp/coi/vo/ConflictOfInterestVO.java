package com.polus.fibicomp.coi.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.CoiConflictStatusType;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiProjectAward;
import com.polus.fibicomp.coi.pojo.CoiProjectProposal;
import com.polus.fibicomp.coi.pojo.CoiProjectType;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelerType;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelType;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.pojo.ValidPersonEntityRelType;
import com.polus.fibicomp.person.pojo.Person;

public class ConflictOfInterestVO {

	private CoiDisclosure coiDisclosure;

	private Person person;

	private Integer numberOfSFI;

	private PersonEntity personEntity;

	private CoiEntity coiEntity;

	private String personId;

	private List<DisclosureDetailDto> proposals;

	private List<DisclosureDetailDto> awards;

	private String searchString;

	private List<EntityStatus> entityStatus;

	private List<EntityType> entityType;

	private List<ValidPersonEntityRelType> validPersonEntityRelTypes;

	private List<CoiConflictStatusType> coiConflictStatusTypes;

	private List<PersonEntityRelationship> personEntityRelationships;

	private PersonEntityRelationship personEntityRelationship;

	private List<PersonEntity> personEntities;

	private Integer moduleCode;

	private Integer moduleItemId;

	private List<CoiDisclEntProjDetails> coiDisclEntProjDetails;

	private Integer disclosureId;

	private Boolean sfiCompleted;

	private Integer conflictIdentifiedCount;

	private Integer newSubmissionsCount;

	private Integer unassignedCount;

	private Integer pendingEntityApproval;

	private Integer reviewCommentsCount;

	private String revisionComment;
	
	private Integer submoduleCode;

	private String disclosureSequenceStatusCode;

	private List<CoiDisclosure> coiDisclosures;

	private Integer coiFinancialEntityId;
	
    private List<CoiSectionsType> coiSectionsType;
	
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

	private CoiDisclEntProjDetails coiDisclEntProjDetail;

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
	
	private List<CoiEntity> coiEntityList;
	
	private Boolean isActive;
	
	private CoiTravelDisclosure coiTravelDisclosure;
	
	private List<CoiTravelDisclosure> coiTravelDisclosureList;

	private List<PersonEntity> personEntityList;

	private List<CoiProjectType> coiProjectTypes;

	private List<PersonEntityRelType> personEntityRelType;

	private Integer personEntityId;
	
	private CoiProjectProposal coiProjectProposal;
	
	private CoiProjectAward coiProjectAward;
	
	private String tabName;
	
	private String disclosureTypeCode;
	
	private Integer travelDisclosureId;
	
	private Integer travelNumber;
    
	private Integer versionNumber;
	
	private String versionStatus;
	
	private Integer entityId;
	
	private Integer entityNumber;

	private String travelStatusCode;
	
	private Boolean isSponsoredTravel;
	
	private String travelTitle;
	
	private String purposeOfTheTrip;
	
	private BigDecimal travelAmount;
	
	private Date travelStartDate;
	
	private Date travelEndDate;
	
	private Integer noOfDays;
	
	private String destinationCity;
	
	private String destinationCountry;
	
	private String relationshipToYourResearch;
	
	private String acknowledgeBy;
	
	private String acknowledgeAt;
	
	private Timestamp updateTimeStamp;
	
	private String updateUser;
	
	private String createUser;
	
	private Timestamp createTimeStamp;

	private String travelState;
	
	private Boolean isInternationalTravel;
	
	private List<CoiTravelerType> CoiTravelerType;

	private Integer travelTravellerId;
	
	private Integer travellerDisclosureId;
	
	private List<String> travellerTypeCode;
	
	private String reviewStatus;
	
	private Integer entityCount;

	public Integer getEntityCount() {
		return entityCount;
	}

	public void setEntityCount(Integer entityCount) {
		this.entityCount = entityCount;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}
	
	public List<String> getTravellerTypeCode() {
		return travellerTypeCode;
	}

	public void setTravellerTypeCode(List<String> travellerTypeCode) {
		this.travellerTypeCode = travellerTypeCode;
	}

	public List<CoiTravelerType> getCoiTravelerType() {
		return CoiTravelerType;
	}

	public void setCoiTravelerType(List<CoiTravelerType> coiTravelerType) {
		CoiTravelerType = coiTravelerType;
	}

	public Boolean getIsInternationalTravel() {
		return isInternationalTravel;
	}

	public void setIsInternationalTravel(Boolean isInternationalTravel) {
		this.isInternationalTravel = isInternationalTravel;
	}

	public String getTravelStatusCode() {
		return travelStatusCode;
	}

	public void setTravelStatusCode(String travelStatusCode) {
		this.travelStatusCode = travelStatusCode;
	}

	public String getTravelState() {
		return travelState;
	}

	public void setTravelState(String travelState) {
		this.travelState = travelState;
	}

	public Integer getTravelTravellerId() {
		return travelTravellerId;
	}

	public void setTravelTravellerId(Integer travelTravellerId) {
		this.travelTravellerId = travelTravellerId;
	}
	
	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public Integer getEntityNumber() {
		return entityNumber;
	}

	public Integer getEntityId() {
		return entityId;
	}

	public Boolean getIsSponsoredTravel() {
		return isSponsoredTravel;
	}

	public String getTravelTitle() {
		return travelTitle;
	}

	public String getPurposeOfTheTrip() {
		return purposeOfTheTrip;
	}

	public Integer getNoOfDays() {
		return noOfDays;
	}

	public String getDestinationCity() {
		return destinationCity;
	}

	public String getDestinationCountry() {
		return destinationCountry;
	}

	public String getRelationshipToYourResearch() {
		return relationshipToYourResearch;
	}

	public String getAcknowledgeBy() {
		return acknowledgeBy;
	}

	public String getAcknowledgeAt() {
		return acknowledgeAt;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public void setEntityNumber(Integer entityNumber) {
		this.entityNumber = entityNumber;
	}

	public Integer getTravelNumber() {
		return travelNumber;
	}

	public void setTravelNumber(Integer travelNumber) {
		this.travelNumber = travelNumber;
	}

	public void setEntityId(Integer entityId) {
		this.entityId = entityId;
	}

	public void setIsSponsoredTravel(Boolean isSponsoredTravel) {
		this.isSponsoredTravel = isSponsoredTravel;
	}

	public void setPurposeOfTheTrip(String purposeOfTheTrip) {
		this.purposeOfTheTrip = purposeOfTheTrip;
	}

	public void setNoOfDays(Integer noOfDays) {
		this.noOfDays = noOfDays;
	}

	public void setDestinationCity(String destinationCity) {
		this.destinationCity = destinationCity;
	}

	public void setDestinationCountry(String destinationCountry) {
		this.destinationCountry = destinationCountry;
	}

	public void setRelationshipToYourResearch(String relationshipToYourResearch) {
		this.relationshipToYourResearch = relationshipToYourResearch;
	}

	public void setAcknowledgeBy(String acknowledgeBy) {
		this.acknowledgeBy = acknowledgeBy;
	}

	public void setAcknowledgeAt(String acknowledgeAt) {
		this.acknowledgeAt = acknowledgeAt;
	}
	public Integer getTravellerDisclosureId() {
		return travellerDisclosureId;
	}

	public void setTravellerDisclosureId(Integer travellerDisclosureId) {
		this.travellerDisclosureId = travellerDisclosureId;
	}
	
	public BigDecimal getTravelAmount() {
		return travelAmount;
	}

	public Date getTravelStartDate() {
		return travelStartDate;
	}

	public Date getTravelEndDate() {
		return travelEndDate;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public String getCreateUser() {
		return createUser;
	}

	public Timestamp getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setTravelTitle(String travelTitle) {
		this.travelTitle = travelTitle;
	}

	public void setTravelAmount(BigDecimal travelAmount) {
		this.travelAmount = travelAmount;
	}

	public void setTravelStartDate(Date travelStartDate) {
		this.travelStartDate = travelStartDate;
	}

	public void setTravelEndDate(Date travelEndDate) {
		this.travelEndDate = travelEndDate;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public void setCreateTimeStamp(Timestamp createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

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

	public PersonEntity getPersonEntity() {
		return personEntity;
	}

	public void setPersonEntity(PersonEntity personEntity) {
		this.personEntity = personEntity;
	}

	public CoiEntity getCoiEntity() {
		return coiEntity;
	}

	public void setCoiEntity(CoiEntity coiEntity) {
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

	public List<EntityStatus> getEntityStatus() {
		return entityStatus;
	}

	public void setEntityStatus(List<EntityStatus> entityStatus) {
		this.entityStatus = entityStatus;
	}

	public List<EntityType> getEntityType() {
		return entityType;
	}

	public void setEntityType(List<EntityType> entityType) {
		this.entityType = entityType;
	}

	public List<ValidPersonEntityRelType> getValidPersonEntityRelTypes() {
		return validPersonEntityRelTypes;
	}

	public void setValidPersonEntityRelTypes(List<ValidPersonEntityRelType> validPersonEntityRelTypes) {
		this.validPersonEntityRelTypes = validPersonEntityRelTypes;
	}

	public List<PersonEntityRelationship> getPersonEntityRelationships() {
		return personEntityRelationships;
	}

	public void setPersonEntityRelationships(List<PersonEntityRelationship> personEntityRelationships) {
		this.personEntityRelationships = personEntityRelationships;
	}

	public PersonEntityRelationship getPersonEntityRelationship() {
		return personEntityRelationship;
	}

	public void setPersonEntityRelationship(PersonEntityRelationship personEntityRelationship) {
		this.personEntityRelationship = personEntityRelationship;
	}

	public List<PersonEntity> getPersonEntities() {
		return personEntities;
	}

	public void setPersonEntities(List<PersonEntity> personEntities) {
		this.personEntities = personEntities;
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

	public List<CoiDisclEntProjDetails> getCoiDisclEntProjDetails() {
		return coiDisclEntProjDetails;
	}

	public void setCoiDisclEntProjDetails(List<CoiDisclEntProjDetails> coiDisclEntProjDetails) {
		this.coiDisclEntProjDetails = coiDisclEntProjDetails;
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

	public String getRevisionComment() {
		return revisionComment;
	}

	public void setRevisionComment(String revisionComment) {
		this.revisionComment = revisionComment;
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
		return coiSectionsType;
	}

	public void setCoiSectionsType(List<CoiSectionsType> coiSectionsType) {
		this.coiSectionsType = coiSectionsType;
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

	public CoiDisclEntProjDetails getCoiDisclEntProjDetail() {
		return coiDisclEntProjDetail;
	}

	public void setCoiDisclEntProjDetail(CoiDisclEntProjDetails coiDisclEntProjDetail) {
		this.coiDisclEntProjDetail = coiDisclEntProjDetail;
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

	public List<CoiEntity> getCoiEntityList() {
		return coiEntityList;
	}

	public void setCoiEntityList(List<CoiEntity> coiEntityList) {
		this.coiEntityList = coiEntityList;
	}

	public CoiTravelDisclosure getCoiTravelDisclosure() {
		return coiTravelDisclosure;
	}

	public void setCoiTravelDisclosure(CoiTravelDisclosure coiTravelDisclosure) {
		this.coiTravelDisclosure = coiTravelDisclosure;
	}

	public List<CoiTravelDisclosure> getCoiTravelDisclosureList() {
		return coiTravelDisclosureList;
	}

	public void setCoiTravelDisclosureList(List<CoiTravelDisclosure> coiTravelDisclosureList) {
		this.coiTravelDisclosureList = coiTravelDisclosureList;
	}

	public List<PersonEntity> getPersonEntityList() {
		return personEntityList;
	}

	public void setPersonEntityList(List<PersonEntity> personEntityList) {
		this.personEntityList = personEntityList;
	}

	public List<CoiProjectType> getCoiProjectTypes() {
		return coiProjectTypes;
	}

	public void setCoiProjectTypes(List<CoiProjectType> coiProjectTypes) {
		this.coiProjectTypes = coiProjectTypes;
	}

	public List<PersonEntityRelType> getPersonEntityRelType() {
		return personEntityRelType;
	}

	public void setPersonEntityRelType(List<PersonEntityRelType> personEntityRelType) {
		this.personEntityRelType = personEntityRelType;
	}

	public Integer getPersonEntityId() {
		return personEntityId;
	}

	public void setPersonEntityId(Integer personEntityId) {
		this.personEntityId = personEntityId;
	}

	public List<CoiConflictStatusType> getCoiConflictStatusTypes() {
		return coiConflictStatusTypes;
	}

	public void setCoiConflictStatusTypes(List<CoiConflictStatusType> coiConflictStatusTypes) {
		this.coiConflictStatusTypes = coiConflictStatusTypes;
	}

	public Boolean getActive() {
		return isActive;
	}

	public void setActive(Boolean active) {
		isActive = active;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public CoiProjectProposal getCoiProjectProposal() {
		return coiProjectProposal;
	}

	public void setCoiProjectProposal(CoiProjectProposal coiProjectProposal) {
		this.coiProjectProposal = coiProjectProposal;
	}

	public CoiProjectAward getCoiProjectAward() {
		return coiProjectAward;
	}

	public void setCoiProjectAward(CoiProjectAward coiProjectAward) {
		this.coiProjectAward = coiProjectAward;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public String getDisclosureTypeCode() {
		return disclosureTypeCode;
	}

	public void setDisclosureTypeCode(String disclosureTypeCode) {
		this.disclosureTypeCode = disclosureTypeCode;
	}
	
}
