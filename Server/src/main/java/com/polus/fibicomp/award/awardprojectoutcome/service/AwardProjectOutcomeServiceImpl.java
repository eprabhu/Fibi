package com.polus.fibicomp.award.awardprojectoutcome.service;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.awardprojectoutcome.dto.AwardOutcomeDTO;
import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.scopusintegration.dao.ScopusDao;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "awardProjectOutcomeService")
public class AwardProjectOutcomeServiceImpl implements AwardProjectOutcomeService {

	protected static Logger logger = LogManager.getLogger(AwardProjectOutcomeServiceImpl.class.getName());

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PrintService printService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private AwardBudgetService awardBudgetService ;

	@Autowired
	private ScopusDao scopusDao;

	@Override
	public String findPublications(CommonVO vo) {
		AwardVO awardVo = new AwardVO();
		awardVo.setPublications(awardProjectOutcomeDao.findPublications(vo));
		awardVo.setPublicationTypes(awardProjectOutcomeDao.getPublicationTypes());
		return commonDao.convertObjectToJSON(awardVo);
	}

	@Override
	public String saveAwardPublication(AwardVO vo) {
		AwardPublications awardPublication = awardProjectOutcomeDao.saveOrUpdateAwardPublications(vo.getAwardPublication());
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardPublication().getAwardId(), vo.getAwardPublication().getUpdateUser());
		return commonDao.convertObjectToJSON(awardPublication);
	}

	@Override
	public String deleteAwardPublication(AwardVO vo) {
		AwardPublications awardPublication = awardProjectOutcomeDao.getAwardPublicationsBasedOnId(Integer.parseInt(vo.getAwardPublicationId()));
		vo.setAwardId(awardPublication.getAwardId());
		awardProjectOutcomeDao.deleteAwardPublication(awardPublication);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardPublication.getAwardId(), vo.getUpdateUser());
		return loadAllAwardProjectOutcomes(vo);
	}

	@Override
	public String loadAllAwardProjectOutcomes(AwardVO vo) {
		vo.setAssociationTypes(awardProjectOutcomeDao.fetchAllAssociationTypes());
		vo.setAwardPublications(awardProjectOutcomeDao.fetchAllAwardPublications(vo.getAwardId()));
		vo.setAwardAssociations(fetchAllAssociations(vo.getAwardId()));
		vo.setAwardAcheivements(awardProjectOutcomeDao.fetchAllAwardAcheivements(vo.getAwardId()));
		if (commonDao.isDynSubSectionEnabled(Constants.SCOPUS_DYN_SUBSECTION_CODE)) {
			vo.setAwardScopuses(scopusDao.fetchAllAwardScopus(vo.getAwardId()));	
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private List<AwardAssociation> fetchAllAssociations(Integer awardId) {
		List<AwardAssociation> awardAssociations = awardProjectOutcomeDao.fetchAllAwardAssociation(awardId);
		awardAssociations.forEach(awardAssociation -> {
			AwardAssociationDetail awardAssociationDetail = awardAssociation.getAwardAssociationDetail();
			if (awardAssociationDetail.getSponsor() != null) {
				awardAssociationDetail.setSponsorName(commonService.getSponsorFormatBySponsorDetail(awardAssociationDetail.getSponsor().getSponsorCode(), awardAssociationDetail.getSponsor().getSponsorName(), awardAssociationDetail.getSponsor().getAcronym()));
			}
			if (awardAssociationDetail.getPrimeSponsor() != null) {
				awardAssociationDetail.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(awardAssociationDetail.getPrimeSponsor().getSponsorCode(), awardAssociationDetail.getPrimeSponsor().getSponsorName(), awardAssociationDetail.getPrimeSponsor().getAcronym()));
			}
		});
		return awardAssociations;
	}

	@Override
	public String saveAwardAssociation(AwardVO vo) {
		AwardAssociation awardAssociation = vo.getAwardAssociation();
		awardAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardAssociation.setUpdateUser(AuthenticatedUser.getLoginUserName());
		AwardAssociationDetail awardAssociationDetail = awardAssociation.getAwardAssociationDetail();
		if (awardAssociationDetail != null) {
			awardAssociationDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardAssociationDetail.setUpdateUser(AuthenticatedUser.getLoginUserName());
			awardAssociationDetail.setAwardAssociation(awardAssociation);
			awardAssociationDetail.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
			awardAssociationDetail.setSponsorName(awardAssociationDetail.getSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(awardAssociationDetail.getSponsor().getSponsorCode(), awardAssociationDetail.getSponsor().getSponsorName(), awardAssociationDetail.getSponsor().getAcronym()) : "");
			awardAssociationDetail.setPrimeSponsorName(awardAssociationDetail.getPrimeSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(awardAssociationDetail.getPrimeSponsor().getSponsorCode(), awardAssociationDetail.getPrimeSponsor().getSponsorName(), awardAssociationDetail.getPrimeSponsor().getAcronym()) : "");
		}
		awardAssociation = awardProjectOutcomeDao.saveOrUpdateAwardAssociation(awardAssociation);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardAssociation.getAwardId(), awardAssociation.getUpdateUser());
		return commonDao.convertObjectToJSON(awardAssociation);
	}

	@Override
	public String deleteAwardAssociation(AwardVO vo) {
		AwardAssociation awardAssociation = awardProjectOutcomeDao.getAwardAssociationBasedOnId(Integer.parseInt(vo.getAwardAssociationId()));
		vo.setAwardId(awardAssociation.getAwardId());
		awardProjectOutcomeDao.deleteAwardAssociation(awardAssociation);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardAssociation.getAwardId(), AuthenticatedUser.getLoginUserName());
		return loadAllAwardProjectOutcomes(vo);
	}

	@Override
	public String addAwardAcheivements(MultipartFile[] files, String formDataJson) {
		AwardVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJson, AwardVO.class);
			AwardAcheivements data = vo.getAwardAcheivement();
			AwardAcheivements awardAcheivement = null;
			List<AwardAcheivements> attachments = awardProjectOutcomeDao.fetchAllAwardAcheivements(data.getAwardId());
			if (!attachments.isEmpty()) {
				for (AwardAcheivements awardAcheivementData : attachments) {
					if (awardAcheivementData.getAwardAcheivementAttachId().equals(data.getAwardAcheivementAttachId())) {
						awardAcheivement = awardAcheivementData;
					}
				}
				if (awardAcheivement == null) {
					awardAcheivement = setAwardAcheivementDetails(awardAcheivement, data);
				}
				if (files.length > 0) {
					if (awardAcheivement.getFileDataId() != null) {
						FileData fileData = commonDao.getFileDataById(awardAcheivement.getFileDataId());
						commonDao.deleteFileData(fileData);
					}
					addAwardAcheivementsAttachmentFile(files, attachments, awardAcheivement, data);
				}
				if (awardAcheivement.getFileName() != null && data.getFileName() == null) {
					FileData fileData = commonDao.getFileDataById(awardAcheivement.getFileDataId());
					commonDao.deleteFileData(fileData);
					awardAcheivement.setFileName(null);
					awardAcheivement.setMimeType(null);
					awardAcheivement.setFileDataId(null);
				}
			} else {
				awardAcheivement = setAwardAcheivementDetails(awardAcheivement, data);
				if (files.length > 0) {
					addAwardAcheivementsAttachmentFile(files, attachments, awardAcheivement, data);
				}
			}
			awardAcheivement.setComment(data.getComment());
			awardAcheivement.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardAcheivement.setUpdateUser(AuthenticatedUser.getLoginUserName());
			awardAcheivement = awardProjectOutcomeDao.saveOrUpdateAwardAcheivements(awardAcheivement);
			vo.setAwardAcheivement(awardAcheivement);
			awardService.updateAwardDocumentUpdateUserAndTimestamp(data.getAwardId(), AuthenticatedUser.getLoginUserName());
		} catch (Exception e) {
			logger.error("Error occured in addAwardAcheivements : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
	}

	private boolean checkForDuplication(String fileName, List<AwardAcheivements> attachments) {
		for (AwardAcheivements attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String deleteAwardAcheivements(AwardVO vo) {
		AwardAcheivements awardAcheivement = awardProjectOutcomeDao.getAwardAcheivementsBasedOnId(Integer.parseInt(vo.getAwardAcheivementId()));
		vo.setAwardId(awardAcheivement.getAwardId());
		if (awardAcheivement.getFileDataId() != null && Boolean.FALSE.equals(awardProjectOutcomeDao.getIsFileDataIdFound(awardAcheivement.getFileDataId()))) {
			FileData fileData = commonDao.getFileDataById(awardAcheivement.getFileDataId());
			commonDao.deleteFileData(fileData);
		}
		awardProjectOutcomeDao.deleteAwardAcheivement(awardAcheivement);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardAcheivement.getAwardId(), AuthenticatedUser.getLoginUserName());
		return loadAllAwardProjectOutcomes(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadAwardAcheivementsAttachment(Integer awardAcheivementId) {
		AwardAcheivements attachment = awardProjectOutcomeDao.getAwardAcheivementsBasedOnId(awardAcheivementId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("Error occured in downloadAwardAcheivementsAttachment : {}", e.getMessage());
		}
		return attachmentData;
	}

	private void addAwardAcheivementsAttachmentFile(MultipartFile[] files, List<AwardAcheivements> attachments, AwardAcheivements awardAcheivement, AwardAcheivements data) {
		File file = new File(files[0].getOriginalFilename());
		try {
			String replaceFileName = file.getName();
			boolean isRenameRequired = false;
			int count = 1;
			isRenameRequired = checkForDuplication(file.getName(),attachments);
			while(isRenameRequired) {
				 replaceFileName = file.getName();
				 replaceFileName = generateFileName(replaceFileName, count);
				 count = count +1;
				 isRenameRequired = checkForDuplication(replaceFileName,attachments);
			}
			if (data.getFileName().equals(file.getName())) {
				awardAcheivement.setFileName(replaceFileName);
				awardAcheivement.setMimeType(files[0].getContentType());
				FileData fileData = new FileData();
				fileData.setAttachment(files[0].getBytes());
				fileData = commonDao.saveFileData(fileData);
				awardAcheivement.setFileDataId(fileData.getFileDataId());
			}
		} catch (Exception e) {
			logger.error("Error occured in addAwardAcheivementsAttachmentFile : {}", e.getMessage());
		}
	}

	private AwardAcheivements setAwardAcheivementDetails(AwardAcheivements awardAcheivement, AwardAcheivements awardAcheivementData) {
		awardAcheivement = new AwardAcheivements();
		awardAcheivement.setAwardId(awardAcheivementData.getAwardId());
		awardAcheivement.setAwardNumber(awardAcheivementData.getAwardNumber());
		awardAcheivement.setSequenceNumber(awardAcheivementData.getSequenceNumber());
		return awardAcheivement;
	}

	@Override
	public String addAwardAcheivementsForWaf(AwardVO vo) {
		try {
			AwardAcheivements data = vo.getAwardAcheivement();
			MultipartFile multipartFile = null;
			MultipartFile[] files = new MultipartFile[1];
			if (vo.getFileContent() != null) {
				multipartFile = commonService.uploadMedia(vo.getFileContent(), data.getFileName(), vo.getRemaining(), vo.getLength(), vo.getFileTimestamp(), vo.getPersonId(), vo.getContentType());
			}
			if (multipartFile != null || vo.getFileContent() == null) {
				files[0] = multipartFile;
				AwardAcheivements awardAcheivement = null;
				List<AwardAcheivements> attachments = awardProjectOutcomeDao.fetchAllAwardAcheivements(data.getAwardId());
				if (!attachments.isEmpty()) {
					for (AwardAcheivements awardAcheivementData : attachments) {
						if (awardAcheivementData.getAwardAcheivementAttachId()
								.equals(data.getAwardAcheivementAttachId())) {
							awardAcheivement = awardAcheivementData;
						}
					}
					if (awardAcheivement == null) {
						awardAcheivement = setAwardAcheivementDetails(awardAcheivement, data);
					}
					if (files[0] != null) {
						if (awardAcheivement.getFileDataId() != null) {
							FileData fileData = commonDao.getFileDataById(awardAcheivement.getFileDataId());
							commonDao.deleteFileData(fileData);
						}
						addAwardAcheivementsAttachmentFile(files, attachments, awardAcheivement, data);
					}
					if (awardAcheivement.getFileName() != null && data.getFileName() == null) {
						FileData fileData = commonDao.getFileDataById(awardAcheivement.getFileDataId());
						commonDao.deleteFileData(fileData);
						awardAcheivement.setFileName(null);
						awardAcheivement.setMimeType(null);
						awardAcheivement.setFileDataId(null);
					}
				} else {
					awardAcheivement = setAwardAcheivementDetails(awardAcheivement, data);
					if (files[0] != null) {
						addAwardAcheivementsAttachmentFile(files, attachments, awardAcheivement, data);
					}
				}
				awardAcheivement.setComment(data.getComment());
				awardAcheivement.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardAcheivement.setUpdateUser(AuthenticatedUser.getLoginUserName());
				awardAcheivement = awardProjectOutcomeDao.saveOrUpdateAwardAcheivements(awardAcheivement);
				vo.setAwardAcheivement(awardAcheivement);
			}
			awardService.updateAwardDocumentUpdateUserAndTimestamp(data.getAwardId(), AuthenticatedUser.getLoginUserName());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getModuleDetails(AwardOutcomeDTO vo) {
		List<ModuleDetails> moduleDetails = new ArrayList<>();
		Integer awardId = vo.getAwardId();
		Integer moduleCode = vo.getModuleCode();
		List<AwardPerson> awardPersons = awardDao.getAwardPersons(awardId);
		List<String> personIds = new ArrayList<>();
		List<Integer> rolodexIds = new ArrayList<>();
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				if (awardPerson.getPersonId() != null) {
					personIds.add(awardPerson.getPersonId());
				} else if (awardPerson.getRolodexId() != null) {
					rolodexIds.add(awardPerson.getRolodexId());
				}
			}
		}
		if (moduleCode.equals(Constants.AWARD_MODULE_CODE)) {
			List<Award> awards = getAllAwardsBasedOnPersons(personIds, rolodexIds, vo.getSearchString(), vo.getAwardNumber());
			if (!awards.isEmpty()) {
				for (Award award : awards) {
					Integer awardIdValue = award.getAwardId();
					ModuleDetails moduleDetail = new ModuleDetails();
					moduleDetail.setModuleId(award.getAwardId());
					moduleDetail.setModuleItemKey(award.getAwardNumber());
					moduleDetail.setModuleStatus(award.getAwardStatus().getDescription());
					moduleDetail.setTitle(award.getTitle());
					AwardPerson awardPerson = awardProjectOutcomeDao.getAwardPiDetails(awardIdValue);
					if (awardPerson != null) {
						moduleDetail.setPiName(awardPerson.getFullName());
						if (awardPerson.getPersonId() != null) {
							moduleDetail.setPiPersonId(awardPerson.getPersonId());
						}
						if (awardPerson.getRolodexId() != null) {
							moduleDetail.setPiRolodexId(awardPerson.getRolodexId());
						}
					}
					if (award.getSponsor() != null) {
						moduleDetail.setSponsorCode(award.getSponsorCode());
						moduleDetail.setSponsorName(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
						moduleDetail.setSponsor(award.getSponsor());
					}
					if (award.getGrantHeaderId() != null) {
						GrantCall grantCall = grantCallDao.fetchGrantCallById(award.getGrantHeaderId());
						if (grantCall!= null && grantCall.getSponsorFundingScheme() != null && grantCall.getSponsorFundingScheme().getFundingScheme() != null && grantCall.getSponsorFundingScheme().getFundingScheme().getSchemeName() != null) {
							moduleDetail.setFundingSchemeCode(grantCall.getSponsorFundingScheme().getFundingScheme().getFundingSchemeCode());
							moduleDetail.setFundingSchemeName(grantCall.getSponsorFundingScheme().getFundingScheme().getSchemeName());
							moduleDetail.setFundingScheme(grantCall.getSponsorFundingScheme().getFundingScheme());
						}
					}
					moduleDetail.setTotalProjectValue(totalAwardGrantedAmount(awardIdValue));
					moduleDetails.add(moduleDetail);
				}
			}
		} else if (moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
			List<Proposal> proposals = getAllProposalsBasedOnPersons(personIds, rolodexIds, vo.getSearchString());
			if (!proposals.isEmpty()) {
				for (Proposal proposal : proposals) {
					ModuleDetails moduleDetail = new ModuleDetails();
					ProposalPerson proposalPerson = awardProjectOutcomeDao.getProposalPiDetails(proposal.getProposalId());
					if (proposalPerson != null) {
						moduleDetail.setPiName(proposalPerson.getFullName());
						if (proposalPerson.getPersonId() != null) {
							moduleDetail.setPiPersonId(proposalPerson.getPersonId());
						}
						if (proposalPerson.getRolodexId() != null) {
							moduleDetail.setPiRolodexId(proposalPerson.getRolodexId());
						}
					}
					moduleDetail.setModuleId(proposal.getProposalId());
					if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
						moduleDetail.setModuleItemKey(proposal.getApplicationId());
					} else {
						moduleDetail.setModuleItemKey(proposal.getProposalId().toString());
					}
					moduleDetail.setModuleStatus(proposal.getProposalStatus().getDescription());
					moduleDetail.setTitle(proposal.getTitle());
					if (proposal.getSponsor() != null) {
						moduleDetail.setSponsorCode(proposal.getSponsorCode());
						moduleDetail.setSponsorName(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
						moduleDetail.setSponsor(proposal.getSponsor());
					}
					if (proposal.getGrantCallId() != null) {
						GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
						if (grantCall!= null && grantCall.getSponsorFundingScheme() != null && grantCall.getSponsorFundingScheme().getFundingScheme() != null && grantCall.getSponsorFundingScheme().getFundingScheme().getSchemeName() != null) {
							moduleDetail.setFundingSchemeCode(grantCall.getSponsorFundingScheme().getFundingScheme().getFundingSchemeCode());
							moduleDetail.setFundingSchemeName(grantCall.getSponsorFundingScheme().getFundingScheme().getSchemeName());
							moduleDetail.setFundingScheme(grantCall.getSponsorFundingScheme().getFundingScheme());
						}
					}
					BudgetHeader budgetHeader = budgetDao.fetchFinalBudget(proposal.getProposalId());
					if (budgetHeader != null) {
						moduleDetail.setTotalProjectValue(budgetHeader.getTotalCost());
					}
					moduleDetails.add(moduleDetail);
				}
			}
		}
		return commonDao.convertObjectToJSON(moduleDetails);
	}

	private List<Award> getAllAwardsBasedOnPersons(List<String> personIds, List<Integer> rolodexIds, String searchString, String awardNumber) {
		List<Integer> awardIds = awardProjectOutcomeDao.getAwardIdBasedOnParams(personIds, rolodexIds);
		List<Award> awards= new ArrayList<>();
		if (awardIds != null && !awardIds.isEmpty()) {
			awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, searchString, awardNumber, null);
		}
		return awards;
	}

	private List<Proposal> getAllProposalsBasedOnPersons(List<String> personIds, List<Integer> rolodexIds,
			String searchString) {
		List<Integer> proposalIds = awardProjectOutcomeDao.getProposalIdBasedOnParams(personIds, rolodexIds);
		List<Integer> proposalStatuses = new ArrayList<>();
		List<Proposal> proposals = new ArrayList<>();
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_INACTIVE);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_REVISION_REQUESTED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_HOD_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_ADMIN_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_MANAGER_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_NOT_SUBMITTED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
		if (proposalIds != null && !proposalIds.isEmpty()) {
			proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, searchString);
		}
		return proposals;
	}

	@Override
	public BigDecimal totalAwardGrantedAmount(Integer awardId) {
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		BigDecimal totalCostShare = BigDecimal.ZERO;
		BigDecimal totalAwardGrantedAmount = BigDecimal.ZERO;
		if (awardCostShares != null && !awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				if (awardCostShare.getCommitmentAmount() != null) {
					totalCostShare = totalCostShare.add(awardCostShare.getCommitmentAmount());
				}
			}
		}
		AwardAmountInfo awardAmountInfo = awardBudgetService.fetchLatestAwardAmountInfo(awardId,awardDao.getAwardNumberBasedOnAwardId(awardId));
		if (awardAmountInfo != null && awardAmountInfo.getAnticipatedTotalAmount() != null) {
			totalAwardGrantedAmount = totalCostShare.add(awardAmountInfo.getAnticipatedTotalAmount());
		} else {
			totalAwardGrantedAmount = totalCostShare;
		}
		return totalAwardGrantedAmount;
	}

}
