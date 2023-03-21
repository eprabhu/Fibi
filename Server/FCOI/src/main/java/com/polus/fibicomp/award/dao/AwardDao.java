package com.polus.fibicomp.award.dao;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dto.AwardHierarchyDto;
import com.polus.fibicomp.award.dto.AwardSearchResult;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardAttachmentType;
import com.polus.fibicomp.award.pojo.AwardBasisOfPayment;
import com.polus.fibicomp.award.pojo.AwardComment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardContactType;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardDocumentType;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardHierarchy;
import com.polus.fibicomp.award.pojo.AwardHistoryLog;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMethodOfPayment;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardReportReminder;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.AwardStatus;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.AwardTransactionType;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.award.pojo.ContactRoleType;
import com.polus.fibicomp.award.pojo.Distribution;
import com.polus.fibicomp.award.pojo.Frequency;
import com.polus.fibicomp.award.pojo.FrequencyBase;
import com.polus.fibicomp.award.pojo.MilestoneStatus;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.pojo.ReportClass;
import com.polus.fibicomp.award.pojo.ReportStatus;
import com.polus.fibicomp.award.pojo.SponsorReport;
import com.polus.fibicomp.award.pojo.SponsorTerm;
import com.polus.fibicomp.award.pojo.SponsorTermReport;
import com.polus.fibicomp.award.pojo.SponsorTermType;
import com.polus.fibicomp.award.pojo.ValidReportClass;
import com.polus.fibicomp.award.vo.AwardDetailsVO;
import com.polus.fibicomp.award.vo.AwardHierarchyVO;
import com.polus.fibicomp.award.vo.AwardSummaryDetailsVO;
import com.polus.fibicomp.award.vo.ReportTermsVO;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service
public interface AwardDao {

	/**
	 * This method is to retrieve details regarding a corresponding award
	 * @param awardId - unique identifier of an award
	 * @return set of values to display award view
	 * @throws Exception
	 */
	public AwardDetailsVO fetchAwardSummaryData(String awardId);

	/**
	 * This method is to retrieve award hierarchy details
	 * @param awardNumber         - number of award
	 * @param selectedAwardNumber - selected award number from the hierarchy
	 * @return set of values to display award view
	 * @throws Exception
	 */
	public AwardHierarchyVO fetchAwardHierarchyData(String awardNumber, String selectedAwardNumber);

	/**
	 * This method is used to get all dropdown values for creating variation
	 * request.
	 * @param vo - Object of CommonVO.
	 * @return A set of dropdown values.
	 */
	public CommonVO getDropDownDatas(CommonVO vo);

	/**
	 * @param award
	 * @return saved Award Object
	 * @throws Exception
	 */
	public Award saveOrUpdateAwardDetails(Award award);

	/**
	 * this method is to fetch award by awardId
	 * @param awardId
	 * @return Award object
	 * @throws Exception
	 */
	public Award getAwardDetailsById(Integer awardId);

	/**
	 * @param vo
	 * @return
	 */
	public AwardSpecialReview saveOrUpdateAwardSpecialReview(AwardSpecialReview specialReview);

	/**
	 * @param awardSpecailReviewId
	 * @return
	 */
	public List<AwardSpecialReview> getAwardSpecialReviewsByAwardId(Integer awardSpecailReviewId);

	/**
	 * This method is to get all subcontracts based on awardId
	 * 
	 * @param awardId
	 * @return
	 */
	public List<AwardSubContract> getSubContractsByAwardId(Integer awardId);

	/**
	 * This method is to save/update Award Cost Share
	 * 
	 * @param awardCostShare
	 * @return
	 */
	public AwardCostShare saveOrUpdateAwardCostShare(AwardCostShare awardCostShare);

	/**
	 * This method is to save or update Award Sub-contracts
	 * 
	 * @param subContract
	 * @return
	 */
	public AwardSubContract saveOrUpdateAwardSubContract(AwardSubContract subContract);

	/**
	 * This method is to remove a special review from an award
	 * 
	 * @param awardSpecailReviewId
	 */
	public void deleteAwardSpecialReview(Integer awardSpecailReviewId);

	/**
	 * This is to remove an award sub contract from database
	 * 
	 * @param subContract
	 */
	public void deleteAwardSubContract(Integer awardSubawardId);

	/**
	 * This is to remove a cost share from database
	 * 
	 * @param awardCostShare
	 */
	public void deleteAwardCostShare(Integer awardCostShareId);
	/**
	 * This method is to save all keywords specific for a particular award
	 * 
	 * @param awardKeyword
	 * @return
	 */
	public AwardKeyword saveOrUpdateAwardKeyword(AwardKeyword awardKeyword);

	/**
	 * @return List of ContactRoleType
	 */
	public List<ContactRoleType> getPersonnelRoleList();

	/**
	 * This method is used to save and update award person details.
	 * 
	 * @param awardPerson - AwardPerson
	 * @return it returns award person Details
	 */
	public AwardPerson saveOrUpdateAwardPersonDetails(AwardPerson awardPerson);

	/**
	 * This method is used to save and update award contact details.
	 * @param awardContact - AwardContact
	 * @return it returns award contact Details
	 */
	public AwardContact saveOrUpdateAwardContactDetails(AwardContact awardContact);

	/**
	 * This method is used to save and update award person details.
	 * @param awardPerson - AwardPerson
	 * @return it returns award person Details
	 */
	public AwardPerson saveAwardKeyPersonnel(AwardPerson awardPerson);

	/**
	 * This method is used to delete award person details.
	 * @param awardPersonId
	 * @return
	 */
	public void deleteAwardPersonnel(Integer awardPersonId);

	/**
	 * This method is used to save and update award project team details.
	 * @param awardProjectTeam - AwardProjectTeam
	 * @return it returns award project team details
	 */
	public AwardProjectTeam saveOrUpdateAwardProjectTeam(AwardProjectTeam awardProjectTeam);

	/**
	 * This method is used to get award contact Type List.
	 * @return it returns award contact type list.
	 */
	public List<AwardContactType> getAwardContactTypeList();

	public List<AwardPerson> getAwardPersonList(Integer awardId);

	public List<AwardContact> getAwardContactList(Integer awardId);

	public List<AwardProjectTeam> getAwardProjectTeamList(Integer awardId);

	public void saveAwardPersonUnits(AwardPersonUnit awardPersonUnit);

	public String getAwardNameByUnitNumber(String unitNumber);

	public String getAwardReportRecipientById(String recipientId);

	public List<ReportClass> getReportClassList();

	public List<Report> getReportList();

	public List<Frequency> getFrequencyList();

	public List<FrequencyBase> getFrequencyBaseList();

	public List<Distribution> getDistributionList();

	public List<SponsorTermType> getSponsorTermTypeList();

	public String deleteAwardSponsorTerm(AwardSponsorTerm awardSponsorTerm);

	public List<SponsorTerm> getSponsorTermList();

	public AwardSponsorTerm saveOrUpdateAwardSponsorTerms(AwardSponsorTerm awardSponsorTerm);

	public AwardReportTerms saveOrUpdateAwardReports(AwardReportTerms awardReport);

	public String deleteAwardReport(Integer awardReportTermsId);

	public void deleteAwardApprovedEquipment(AwardApprovedEquipment awardApprovedEquipment);

	public void deleteAwardAprovedForeignTravel(AwardAprovedForeignTravel awardAprovedForeignTravel);

	public AwardApprovedEquipment saveOrUpdateAwardApprovedEquipment(AwardApprovedEquipment awardApprovedEquipment);

	public AwardAprovedForeignTravel saveOrUpdateAwardAprovedForeignTravel(AwardAprovedForeignTravel awardAprovedForeignTravel);

	public String deleteAwardReportRecepientById(AwardReportTermRecipient awardReportTermRecipient);

	public String deleteAwardPersonnelUnitByUnitId(AwardPersonUnit awardPersonUnit);

	public SponsorTerm getSponsorTermByCode(String sponsorTermCode, String sponsorTermTypeCode);

	public SponsorTermType getSponsorTermTypeByCode(String sponsorTermTypeCode);

	public void getAwardSponsorTerms(Integer awardId, ReportTermsVO reportTermsVO);

	public void getAwardSponsorReports(Integer awardId, ReportTermsVO reportTermsVO);

	/**
	 * This method is used to update the award number and return the updated number return
	 * @return new award number
	 */
	public Integer updateAwardNextValue();

	/**
	 * This method is used to fetch institutional proposal by id.
	 * @param proposalId - Id of the institute proposal.
	 * @return corresponding InstituteProposal object.
	 */
	public InstituteProposal fetchInstProposalById(Integer proposalId);

	/**
	 * This method is used to fetch development proposal by institute proposal.
	 * @param proposalId - Id of the institute proposal.
	 * @return corresponding development proposal id of institute proposal.
	 */
	public Integer fetchDevProposalByInstProposal(Integer proposalId);

	public void feedAwardBudgetFromIP(Integer proposalId, Integer awardId, String updateUser);

	public AwardFundingProposal saveOrUpdateFundingProposal(AwardFundingProposal awardFundingProposal);

	public void deleteFundingProposal(Integer awardFundingProposalId);
	
	public Boolean checkIpLinkedInAwards(Integer awardFundingProposalId);

	public List<AwardFundingProposal> getAwardFundingProposals(Integer awardId);

	public AwardAttachment saveAttachment(AwardAttachment awardAttachment);

	public void deleteAwardAttachment(Integer awardAttachmentId);

	public List<AwardType> fetchAllAwardTypes();

	public List<AwardReportTracking> getReportTrackingDetails(Integer awardId, Integer awardReportTermsId);

	public List<ReportStatus> getReportStatusList();

	public AwardReportTerms getAwardReportTermsById(Integer awardReportTermsId);

	public Frequency getFrequencyByFrequencyCode(String frequencyCode);

	public ReportStatus getPendingReportStatus(String statusCode);

	public void deleteAwardReportTrackingById(AwardReportTerms awardReportTerms,  List<Integer> awardReportTrackingIds);

	public List<AwardAttachmentType> fetchAllAwardAttachmentTypes();

	public List<AwardAttachment> getAwardAttachmentsByAwardId(Integer awardId, Boolean isPersonHasPermission);

	public AwardAttachment fetchAwardAttachmentById(Integer awardAttachmentId);

	public List<AwardAttachment> fetchSortedAwardAttachments(Integer awardId, String sortBy, String reverse);

	public void deleteAwardTransaction(AwardAmountInfo awardAmountInfo);

	public void deleteAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction);

	public AwardPerson saveOrUpdateAwardPersons(AwardPerson awardPersons);

	public AwardPersonUnit saveOrUpdateAwardPerson(AwardPersonUnit awardPersonUnit);

	public String getAwardName(Integer awardId);

	public List<AwardHierarchyDto> fetchAllawardHierarchy(String awardNumber, String selectedAwardNumber);

	public Award fetchAwardByAwardId(String awardId);

	public String saveOrUpdateAwardHierarchy(AwardHierarchy awardHierarchy);

	public String getNextChildAwardNumber(String awardNumber);

	public List<AwardHierarchy> getChildAwards(String parntAwardNumber);

	public void deleteChildAwards(AwardHierarchy awardHierarchy);

	public void deleteAwardByAwardNumber(String awardNumber);

	public List<AwardHierarchy> getAwardHierarchyByAwardNumber(String awardNumber);

	public List<AwardPerson> fetchAllAwardPesonsByAwardId(String awardId);

	public Award saveAward(Award award);

	public AwardStatus fetchAwardStatusByCode(String statusCode);

	public List<AwardStatus> fetchAllAwardStatus();

	/**
	 * This method is used to fetch award view based on award id.
	 * @param awardId - awardId.
	 * @return list of award views.
	 */
	public AwardView fetchAwardViewByAwardId(Integer awardId);

	public void deleteAwardProjectTeam(Integer awardPersonId);

	public List<AwardAttachment> fetchAwardAttachmentBasedOnAwardIdAndDocumentId(Integer awardId, Integer documentId);

	public AwardContact saveOrUpdateAwardContact(AwardContact awardContact);

	public void deleteAwardContact(Integer awardPersonId);

	/**
	 * This method is used to fetch the award person by awardPersonId
	 * @param awardPersonalId
	 * @return object of corresponding AwardPerson
	 */
	public AwardPerson getAwardPersonById(Integer awardPersonalId);

	/**
	 * This method is used to download award person attachments
	 * @param attachmentid
	 * @return
	 */
	public AwardPersonAttachment fetchAwardPersonAttachmentById(Integer attachmentid);

	/**
	 * This method is used to fetch the award type by award type code
	 * @param awardTypeCode
	 * @return object of corresponding AwardType
	 */
	public AwardType fetchAwardTypeByAwardTypeCode(String awardTypeCode);

	/**
	 * This method is used to fetch the award document type by award type code
	 * @param awardTypeCode
	 * @return object of corresponding AwardType
	 */
	public AwardDocumentType fetchAwardDocumentTypeById(String awardDocumentTypeCode);

	/**
	 * This method is to get Award ReportTerms based on awardId
	 * @param awardId
	 * @return
	 */
	public List<AwardReportTerms> getAwardReportTermsByAwardId(Integer awardId);

	/**
	 * This method is to get Award AwardNumber And SequenceNumber
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return award object
	 */
	public Award fetchAwardByAwardNumberAndSequenceNumber(String awardNumber, int sequenceNumber);

	/**
	 * This method is to get Award Sponsor Term based on awardId
	 * @param awardId
	 * @return List of Award Sponsor Terms
	 */
	public List<AwardSponsorTerm> getAwardSponsorTermByAwardId(Integer awardId);

	/**
	 * This method is to get Award based on awardNumber
	 * @param awardNumber
	 * @return List of Award
	 */
	public List<Award> fetchAwardByAwardNumber(String awardNumber);

	/**
	 * This method is to fetch Award By AwardNumber And Award Sequence Status
	 * @param awardSequenceStatus
	 * @param awardNumber
	 * @return Award
	 */
	public Award fetchAwardByAwardNumberAndAwardSequenceStatus(String awardNumber, List<String> awardSequenceStatuses);
	
	/**
	 * This method is used to prevent duplicate entry hierarchy table
	 * @param award
	 * @return
	 */
	public List<AwardHierarchy> checkAwardHierarchyExisted(Award award);

	/**
	 * This method is to get All Service Requests based on award is
	 * @param awardId - awardId
	 * @param originatedAwardId 
	 * @return Object of ServiceRequest
	 */
	public ServiceRequest getServiceRequestBasedOnAwardId(String awardId, String originatedAwardId);

	/**
	 * This method is to fetch Sponsor Report Based On FundingSourceType
	 * @param fundingSchemeId - fundingSchemeId
	 * @param sponsorCode - sponsorCode
	 * @return List of SponsorReport
	 */
	public List<SponsorReport> fetchSponsorReportBasedOnFundingSourceType(Integer fundingSchemeId, String sponsorCode);

	/**
	 * This method is to fetch Sponsor Term Report Based On FundingSourceType
	 * @param fundingSchemeId - fundingSchemeId
	 * @param sponsorCode - sponsorCode
	 * @return List of SponsorTermReport
	 */
	public List<SponsorTermReport> fetchSponsorTermReportBasedOnFundingSourceType(Integer fundingSchemeId, String sponsorCode);

	/**
	 * This method is to fetch Sponsor Report Based On Sponsor
	 * @param sponsorCode - sponsorCode
	 * @return List of SponsorReport
	 */
	public List<SponsorReport> fetchSponsorReportBasedOnSponsor(String sponsorCode);

	/**
	 * This method is to fetch Sponsor Term Report Based On Sponsor
	 * @param Sponsor - Sponsor
	 * @return List of SponsorTermReport
	 */
	public List<SponsorTermReport> fetchSponsorTermReportBasedOnSponsor(String sponsorCode);

	/**
	 * This method is used to fetch the getProposalId 
	 * @param ipNumber
	 * @return Integer proposalID
	 */
	public Integer getProposalId(String ipNumber);

	/**
	 * This method is used to fetch Person Attachment By Id
	 * @param attachmentId - attachmentId
	 * @return object of AwardPersonAttachment
	 */
	public AwardPersonAttachment fetchPersonAttachmentById(Integer attachmentId);

	/**
	 * This method is used to delete Award Person Attachment
	 * @param attachmentId - attachmentId
	 */
	public void deleteAwardPersonAttachment(Integer attachmentId);

	/**
	 * This method is used to delete Award Person Unit
	 * @param awardPersonUnit - object of awardPersonUnit
	 */
	public void deleteAwardPersonUnit(AwardPersonUnit awardPersonUnit);

	/**
	 * This method is used to show all awards except the latest based on award number
	 * @param awardNumber
	 * @return list of awards
	 */
	public List<Award> showAwardHistory(String awardNumber);

	/**
	 * This method is to AwardAprovedForeignTravel Based On AwardId
	 * @param awardId - awardId
	 * @return List of AwardAprovedForeignTravel
	 */
	public List<AwardAprovedForeignTravel> getAwardAprovedForeignTravelByAwardId(Integer awardId);

	/**
	 * This method is to AwardApprovedEquipment Based On AwardId
	 * @param awardId - awardId
	 * @return List of AwardApprovedEquipment
	 */
	public List<AwardApprovedEquipment> getAwardApprovedEquipmentByAwardId(Integer awardId);

	/**
	 * This method is used to fetch Award Person Roles based on awardId.
	 * @param awardId - Id of the award.
	 * @return Award person roles list.
	 */
	public List<AwardPersonRoles> fetchAwardPersonRoles(Integer awardId);

	/**
	 * This method is used to fetch award roles.
	 * @return list of roles.
	 */
	public List<Role> fetchAwardRoles();

	/**
	 * This method is used to save award person role.
	 * @param personRole.
	 * @return personRole data.
	 */
	public AwardPersonRoles saveOrUpdateAwardPersonRoles(AwardPersonRoles awardPersonRole);

	/**
	 * This method is used to delete award person role.
	 * @param personRole.
	 * @return personRole data.
	 */
	public AwardPersonRoles deleteAwardPersonRoles(AwardPersonRoles awardPersonRole);

	/**
	 * This method is used to fetch CreateVariatioRequest right Based On PersonId And awardId.
	 * @param awardPersonId - awardPersonId
	 * @param awardId - Id of awardId.
	 * @param roleIds - List of roleId
	 * @return object of AwardPersonRoles.
	 */
	public List<AwardPersonRoles> fetchAwardPersonRolesByParams(String awardPersonId, Integer awardId, List<Integer> roleIds);

	/**
	 * This method is used to fetchAwardPersonRoleBasedonAwardPersonRoleId
	 * @param awardPersonId - Id of the awardPerson.
	 * @return object of AwardPersonRoles.
	 */
	public AwardPersonRoles fetchAwardPersonRoleBasedonAwardPersonRoleId(Integer awardPersonalId);

	/**
	 * This method is used to save award report tracking file
	 * @param awardReportTrackingFile - awardReportTrackingFile
	 * @return object of AwardReportTrackingFile
	 */
	public AwardReportTrackingFile saveOrUpdateAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile);

	public List<AwardReportTrackingFile> fetchAwardReportTrackingFileBasedOnAwardReportTrackingId(Integer awardReportTrackingId);

	/**
	 * This method is to AwardReportTrackingFile Based On AwardId
	 * @param awardReportTrackingFileId - awardReportTrackingFileId
	 * @return List of AwardReportTrackingFile
	 */
	public AwardReportTrackingFile getAwardReportTrackingFileByFileId(Integer awardReportTrackingFileId);

	/**
	 * This method is to get award from sequence id and award number
	 * @param sequenceNumber - sequenceNumber
	 * @param awardNumber - awardNumber
	 * @return List of Award
	 */
	public Award getAwardFromSequenceIdAndAwardNumber(Integer sequenceNumber, String awardNumber);

	/**
	 * This method is to delete Award Report Tracking File
	 * @param awardReportTrackingFile - object of awardReportTrackingFile
	 * @return Object of awardReportTrackingFile 
	 */
	public  AwardReportTrackingFile deleteAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile);

	public Integer getMaxSequenceNumberBasedOnAwardNumber(String awardNumber);

	/**
	 * This method is to get Award AwardNumber And SequenceNumber
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return award object
	 */
	public Award fetchActiveAwardByAwardNumber(String awardNumber);

	/**
	 * This method is to get Award AwardNumbe
	 * @param awardId
	 * @return award Number
	 */
	public String getAwardNumberBasedOnAwardId(Integer awardId);

	public List<ValidReportClass> fetchValidReportClassByReportClassCode(String reportClassCode);

	public String getReportNameByReportCode(String reportCode);

	public AwardTransactionType getAwardTransactionTypeById(Integer awardTransactionTypeCode);

	/**
	 * This method is used to fetch the getProposalIdByIpNumber 
	 * @param ipNumber
	 * @return Integer proposalID
	 */
	public Integer getProposalIdByIpNumber(String ipNumber);

	public Award fetchPendingAwardByAwardNumber(String awardNumber);

	/**
	 * This method is to get Award based on params
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return award object
	 */
	public Award fetchAwardByParams(String awardNumber, int sequenceNumber);

	/**
	 * This method is to fetch All Hold Awards.
	 * @return List of Awards
	 */
	public List<Award> fetchAllHoldAwards();

	/**
	 * This method is to fetch All Active Award Report Reminder.
	 * @return List of AwardReportReminder
	 */
	public List<AwardReportReminder> fetchAllActiveAwardReportReminder();

	/**
	 * This method is to fetch All Award Report Terms Based On Params.
	 * @param reportClassCode - reportClassCode
	 * @param reportCode - reportCode
	 * @param frequencyCode - frequencyCode
	 * @return List of AwardReportReminder
	 */
	public List<AwardReportTerms> fetchAllAwardReportTermsBasedOnParams(String reportClassCode, String reportCode, String frequencyCode);

	/**
	 * This method is to get Report Class By Code.
	 * @param reportClassCode - reportClassCode
	 * @return Object of ReportClass
	 */
	public ReportClass getReportClassByCode(String reportClassCode);

	/**
	 * This method is to fetch All AwardMethodOfPayment.
	 * @return List of AwardMethodOfPayment
	 */
	public List<AwardMethodOfPayment> getAwardMethodOfPaymentList();

	/**
	 * This method is to fetch All AwardBasisOfPayment.
	 * @return List of AwardMethodOfPayment
	 */
	public List<AwardBasisOfPayment> getAwardBasisOfPaymentList();

	/**
	 * This method is to fetch  AwardMethodOfPayment by Id.
	 * @param methodOfPaymentCode
	 * @return Object of AwardMethodOfPayment
	 */
	public AwardMethodOfPayment getAwardMethodOfPaymentById(String methodOfPaymentCode);

	/**
	 * This method is to fetch  Award Basis Of Payment by Id.
	 * @param basisOfPaymentCode
	 * @return Object of AwardBasisOfPayment
	 */
	public AwardBasisOfPayment getAwardBasisOfPaymentById(String basisOfPaymentCode);

	/**
	 * This method is used to fetch Award MileStone Based On Award Id.
	 * @param awardId - awardId of the Award.
	 * @return An object of AwardMileStone.
	 */
	public List<AwardMileStone> fetchAwardMileStonesBasedOnAwardId(Integer awardId);

	/**
	 * This method is used to save AwardMilestone.
	 * @param awardMilestone - AwardMilestone object.
	 * @return An object of AwardMilestone.
	 */
	public AwardMileStone saveOrUpdateAwardMileStone(AwardMileStone awardMileStone);

	/**
	 * This method is used to delete Award milestone based on id.
	 * @param awardMilestoneId - Id of the Award Milestone.
	 * @return success message.
	 */
	public String deleteAwardMilestone(Integer awardMilestoneId);

	/**
	 * This method is used to save and update Award KPIs
	 * @param AwardKPI - awardKPI
	 * @return object of AwardKPI.
	 */
	public AwardKPI saveOrUpdateAwardKPI(AwardKPI awardKPI);

	/**
	 * This method is used to fetch all Award KPIs based on params.
	 * @param awardId
	 * @return list of award kpi.
	 */
	public List<AwardKPI> fetchAllAwardKPI(Integer awardId);

	/**
	 * This method is used to delete Award KPI based on params.
	 * @param awardId  
	 * @param awardKPIId 
	 * @param awardKPICriteriaId 
	 * @return success message.
	 */
	public String deleteAwardKPI(Integer awardKPIId, Integer awardId, Integer awardKPICriteriaId);

	/**
	 * This method is to get Awards based on awardNumber and not in sequenceStatus
	 * @param awardNumber
	 * @param sequenceStatus
	 * @return List of Award
	 */
	public Set<Integer> fetchAwardIdsByAwardNumberAndNotInSequenceStatus(String awardNumber, String sequenceStatus);

	/**
	 * This method is to fetch Award Attachment Based  On AttachmentIds
	 * @param AttachmentIds - List of AttachmentIds
	 * @return List of AwardAttachment
	 */
	public List<AwardAttachment> fetchAwardAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds);

	/**
	 * This method is used to saveOrUpdateAwardResearchArea.
	 * @param awardResearchArea -object of the AwardResearchArea.
	 * @return An object of AwardResearchArea.
	 */
	public AwardResearchArea saveOrUpdateAwardResearchArea(AwardResearchArea awardResearchArea);

	/**
	 * This method is used to fetch AwardResearchArea Based On Award Id.
	 * @param awardId - Award Id of the award.
	 * @return An List of AwardResearchArea.
	 */
	public List<AwardResearchArea> fetchAwardResearchAreaBasedOnAwardId(Integer awardId);

	/**
	 * This method is used to fetch AwardResearchArea Based On Id.
	 * @param researchAreaId - Id of the AwardResearchArea.
	 * @return An Object of AwardResearchArea.
	 */
	public AwardResearchArea fetchAwardResearchArea(Integer researchAreaId);

	/**
	 * This method is used to deleteAwardResearchArea.
	 * @param awardResearchArea - Object of the awardResearchArea.
	 * @return An Object of AwardResearchArea.
	 */
	public AwardResearchArea deleteAwardResearchArea(AwardResearchArea awardResearchArea);

	/**
	 * This method is used to fetch award list based on statusCode
	 * @param awardWorkflowStatusCode - statusCode of awardWorkflow.
	 * @return List of awards.
	 */
	public List<Award> fetchAwardByStatusCode(String awardWorkflowStatusCode);

	/**
	 * This method is used to fetch award list based on statusCode
	 * @param awardWorkflowStatusCode - statusCode of awardWorkflow.
	 * @return object of AwardWorkflowStatus.
	 */
	public AwardWorkflowStatus fetchAwardWorkflowStatusByStatusCode(String awardWorkflowStatusCode);

	/**
	 * This method is used to fetch grant call details by award Id
	 * @param awardId - Id of award.
	 * @return object of Award.
	 */
	public Award fetchGrantCallByAwardId(Integer awardId);

	/**
	 * This method is used to fetch award person roles based on awardId.
	 * @param awardId - Id of the award.
	 * @param roleId - Id of the Role.
	 * @return Award person roles list.
	 */
	public List<AwardPersonRoles> fetchAwardPersonRoles(Integer awardId, Integer roleId);

	/**
	 * This method is used to fetch invoice frequency list.
	 */
	public List<Frequency> getInvoiceFrequencyList();

	/**
	 * This method is used to fetch award lead unit number based on awardId.
	 * @param awardId - Id of the award.
	 * @return lead unit number.
	 */
	public String fetchAwardLeadUnitNumberByAwardId(Integer awardId);

	/**
	 * This method is used to save AwardComment.
	 * @param awardComment - AwardComment object.
	 * @return An object of AwardComment.
	 */
	public AwardComment saveOrUpdateAwardComment(AwardComment awardComment);

	/**
	 * This method is used to fetch award sequence number based on awardId.
	 * @param awardId - Id of the award.
	 * @return sequence number.
	 */
	public Integer getAwardSequenceNumberBasedOnAwardId(Integer awardId);

	/**
	 * This method is used to get Previous AwardIds Based On Params.
	 * @param awardNumber 
	 * @param awardSequenceNumber
	 * @return List of award ids.
	 */
	public List<Integer> getPreviousAwardIdsBasedOnParams(String awardNumber, Integer awardSequenceNumber);

	/**
	 * This method is used to get Award Attachments By AwardIds.
	 * @param awardIds 
	 * @return List of Award Attachment.
	 */
	public List<AwardAttachment> getAwardAttachmentsByAwardIds(List<Integer> awardIds);

	/**
	 * This method is to fetch Award Report Tracking Based On ReportTermIds
	 * @param reportTermIds - List of reportTermIds
	 * @return List of AwardReportTracking
	 */
	public List<AwardReportTracking> fetchAwardReportTrackingBasedOnReportTermIds(Set<Integer> reportTermIds);

	/**
	 * This method is to fetch Award Person Attachment Based  On Award Id
	 * @param awardId - AwardId
	 * @return List of AwardPersonAttachment
	 */
	public List<AwardPersonAttachment> fetchAwardPersonAttachmentBasedOnAwardId(Integer awardId);

	/**
	 * This method is used to fetch AwardAttachmentType by awardAttachmentTypeId
	 * @param awardAttachmentTypeId - Id of awardAttachmentType.
	 * @return object of AwardAttachmentType.
	 */
	public AwardAttachmentType getAwardAttachmentTypeById(String awardAttachmentTypeId);

	/**
	 * This method is used to get the award persons based on the awardId
	 * @param awardId
	 * @return list of award persons
	 */
	public List<AwardPerson> getAwardPersons(Integer awardId);

	/**
	 * This method is used to get the award by params
	 * @param accountNumber
	 * @param awardSequenceStatus
	 * @return object of award
	 */
	public Award fetchAwardDetailsbyAccountNumber(String accountNumber, String awardSequenceStatus);

	/**
	 * @param awardNumber
	 * @param awardSequenceStatuses
	 * @return List of AwardSummaryDetailsVO
	 */
	public List<AwardSummaryDetailsVO> fetchAwardByAwardNumbersAndAwardSequenceStatus(String awardNumber, List<String> awardSequenceStatuses);

	/**
	 * This method is used to getAccountNumberByAwardId
	 * @param awardId
	 * @return Account Number
	 */
	public String getAccountNumberByAwardId(Integer awardId);

	/**
	 * @param awardNumber
	 * @return Max Document ID
	 */
	public Integer getMaxDocumentIdBasedOnAwardNumber(String awardNumber);

	/**
	 * @param awardNumber
	 * @param documentId
	 * @return boolean Value
	 */
	public Boolean checkDocumentIdExistInAward(String awardNumber, Integer documentId);

	/**
	 * @param awardId
	 * @param awardNumber
	 * @param awardSequenceNumber
	 * @param isPersonHasPermission 
	 * @return List of AwardAttachment
	 */
	public List<AwardAttachment> getAwardAttachmentsBasedOnParams(Integer awardId, String awardNumber, Integer awardSequenceNumber, Boolean isPersonHasPermission);

	/**
	 * @param awardNumber
	 * @return object of Award
	 */
	public Award fetchAwardSetUpByAwardNumber(String awardNumber);

	/**
	 * @param awardId
	 * @param updateUser
	 */
	public void createOrUpdateMasterAward(Integer awardId, String updateUser);

	/**
	 * @param awardNumber
	 * @return awardId
	 */
	public Integer getLatestArchiveAwardId(String awardNumber);

	/**
	 * @param awardHistoryLog
	 */
	public void saveOrUpdateAwardHistoryLog(AwardHistoryLog awardHistoryLog);

	/**
	 * @param awardId
	 * @return object of AwardHistoryLog
	 */
	public AwardHistoryLog getAwardHistoryLogBasedOnAwardId(Integer awardId);

	/**
	 * @param awardId
	 * @return awardSequenceStatus
	 */
	public String getAwardSequenceStatusByAwardId(Integer awardId); 

	/**
	 * @param awardId
	 */
	public void updateAwardDocumentUpdateUserAndTimestamp(Integer awardId);

	/**
	 * This method is used to check right in award
	 * @param personId
	 * @param rightName
	 * @param awardId
	 * @return 
	 */
	public Boolean checkPersonHasRightInAward(String personId, String rightName, Integer awardId);

	/**
	 * @param awardId
	 * @param variationTypeCode
	 * @return boolean value
	 */
	public String canCreateVariationRequest(Integer awardId, String variationTypeCode);

	/**
	 * @param awardNumber
	 * @param fileName
	 * @param awardAttachmentLetterOfAcceptance
	 * @return Object Of AwardAttachment
	 */
	public AwardAttachment getAwardAttachmentDetailsBasedOnParams(String awardNumber, String fileName, String awardAttachmentLetterOfAcceptance);

	/**
	 * @param awardId
	 * @return
	 */
	public Boolean isDatesAndAmountEditable(Integer awardId);

	/**
	 * @param awardId
	 * @return
	 */
	public String getAwardDocumentTypeCode(Integer awardId);

	/**
	 * @return id of award End date is passed while running scheduler
	 */
	public List<Integer> fetchAllExpiringAwardIds();

	/**
	 * @param List of award Ids
	 */
	public void updateAwardStatusToExpired(List<Integer> filteredAwardIds);

	/**
	 * This method is used to get award document type code
	 * @param awardId
	 * @return object of AwardDocumentType
	 */
	public AwardDocumentType getAwardDocumentTypeByAwardId(Integer awardId);
	
	/**
	 * @param awardReportTrackingId
	 * @param versionNumber								   
	 */
	public void archiveOldAttachmentVersion(Integer awardReportTrackingId, int versionNumber);

	/**
	 * This method is used to get award report tracking attachment versions
	 * @param awardReportTrackingId
	 * @return list of attachment else [] array
	 */
	public List<AwardReportTrackingFile> getReportTrackingAttachmentVersions(Integer awardReportTrackingId);

	/**
	 * This method is used to get award report tracking attachment
	 * @param trackingId
	 * @return list of attachment else []
	 */
	public List<AwardReportTrackingFile> getAllDraftReportTrackingAttachments(List<Integer> trackingId);

	/**
	 * This method is used to update report terms
	 * @param awardReportTracking
	 * @return updated report terms
	 */
	public AwardReportTracking saveOrUpdateReportTracking(AwardReportTracking awardReportTracking);

	/**
	 * This method is used to delete report terms
	 * @param awardReportTrackingId
	 * @return success message
	 */
	public void deleteReportTracking(Integer awardReportTrackingId);

	/**
	 * @param fileId
	 * @return
	 */
	public Boolean getIsFileDataIdFound(String fileId);

	/**
	 * @param awardReportTrackingIds
	 */
	public void deleteAwardReportTrackingFileByTrackingId(List<Integer> awardReportTrackingIds);

	/**
	 * @param awardReportTrackingFileId
	 * @param reportTrackingId
	 * @param reportTermsId
	 */
	public void deleteReportTrackingAttachment(Integer awardReportTrackingFileId, Integer reportTrackingId, Integer reportTermsId);

	/**
	 * @param awardNumber
	 * @return list of awardIds
	 */
	public List<Integer> getListOfPendingAwardForAwardNumber(String awardNumber);

	/**
	 * @param awardId
	 * @param frequencyBaseProjectEndDate
	 * @return list of report tracking
	 */
	public List<AwardReportTracking> getAwardReportTrackingByParameters(Integer awardId,
			String frequencyBaseProjectEndDate);

	/**
	 * This method is to get Award AwardNumber And SequenceNumber
	 * @param awardNumbers
	 */
	public List<Award> fetchActiveAwardByAwardNumbers(List<String> awardNumbers);
	
	/**
	 * @param awardId
	 */
	public void deleteAwardReportByAwardId(Integer awardId);

	/**
	 * @param awardId
	 */
	public void deleteAwardSponsorTermsByAwardId(Integer awardId);

	/**
	 * @param awardId
	 * @return boolean
	 */
	public Boolean checkAwardReportTermsExist(Integer awardId);

	/**
	 * @param awardId
	 * @return boolean
	 */
	public Boolean checkAwardSponsorTermsExist(Integer awardId);

	/**
	 * This method is used to fetch the award based on the given search string
	 * @param searchString
	 * @return
	 */
	public Set<AwardSearchResult> findAward(String searchString);

	/**
   * @param awardNumber
	 * @return active awardNumber
	 */
	public Integer fetchActiveAwardIdByAwardNumber(String awardNumber);

	/**
	 * @param awardId
	 * @param awardNumber
	 * @return
	 */
	public SapFeedStatus fetchFeedStatusBasedOnAward(Integer awardId, String awardNumber);

	/**
	 * @param awardId
	 * @param statusCode
	 */
	public void updateAwardStatusByStatusCode(Integer awardId, String statusCode);

	/**
	 * @param awardNumber
	 * @return
	 */
	public String fetchActiveAwardLeadUnitByAwardNumber(String awardNumber);

	/**
	 * @param awardNumber
	 * @return
	 */
	public List<String> getAwardVariationsBasedOnAwardNumber(String awardNumber);

	/**
	 * This method is used to getAwardActivityTypeCodeByAwardId
	 * @param awardId
	 * @return Activity Type code
	 */
	public String getAwardActivityTypeCodeByAwardId(Integer awardId);

	/**
	 * @param awardId
	 * @return
	 */
	public Award getAwardSequenceNumberAndSeqStatusByAwardId(Integer awardId);

	/**
	 * @param awardNumber
	 * @return
	 */
	public void deleteAwardByAwardID(Integer awardID);
    
	/**
	 * @param awardId
	 */
	public Integer checkChildAwardExisted(String awardNumber);
    
	/**
	 * @param awardNumber
	 */
	public String getAwardCreateUser(String awardNumber);
	
	/**
	 * This method is used for getAwardMilestoneStatus
	 */
	public List<MilestoneStatus> getAwardMilestoneStatus();

	/**
	 * @param awardId
	 */
	public void deleteAwardReportTrackingAfterEndDate(Integer awardId);

	/**
	 * * This method is used for fetch AwardBaseDates using awardId
	 * @param awardId
	 */
	public Map<String, Timestamp> getAwardBaseDates(Integer awardId);

	/**
	 * * This method is used for checking if  base Date need to included as first due date for award report term
	 * @param frequencyBaseCode
	 */
	public Boolean fetchBaseDateIncludeStatus(String frequencyBaseCode);

	/**
	 * * This method is used for getting award history details based on award number
	 * @param awardNumber
	 */
	public List<Award> getAwardHistoryDetails(String awardNumber);

	/**
	 * * This method is used for getting Service Request Details based on moduleCode & originatingModuleItemKeys
	 * @param moduleCode
	 * @param originatingModuleItemKeys
	 */
	public List<ServiceRequest> getServiceRequestDetailsBasedOnModuleCodeAndOriginatingModuleItemKeys(Integer moduleCode, List<String> originatingModuleItemKeys);

	/**
	 * This method is used to get the service request based on originated award id
	 * @param awardId
	 * @return service request
	 */
	public ServiceRequest getServiceRequestBasedOnOriginatedAwardId(String awardId);

	public List<AwardPerson> getAwardPersonListForTerms(Integer awardId);

}
