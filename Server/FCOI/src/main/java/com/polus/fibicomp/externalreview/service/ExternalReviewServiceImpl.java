package com.polus.fibicomp.externalreview.service;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externalreview.dao.ExternalReviewServiceDao;
import com.polus.fibicomp.externalreview.pojo.*;
import com.polus.fibicomp.grantcall.dao.GrantCallScoringDao;
import com.polus.fibicomp.grantcall.module.GrantCallModuleDao;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Transactional
@Service(value = "ExternalReviewService")
public class ExternalReviewServiceImpl implements ExternalReviewService {

    private static final int DEFAULT_ATTACHMENT_TYPE_CODE = 1;
    private static final int SEND_FOR_REVIEW_STATUS_CODE = 2;
    private static final int EXT_REVIEW_DEFAULT_STATUS_CODE = 1;
    private static final int PROPOSAL_GRATCALL_TYPE_CODE = 2;

    @Autowired
    private ExternalReviewServiceDao reviewServiceDao;

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private QuestionnaireDAO questionnaireDAO;

    @Autowired
    private GrantCallScoringDao grantCallScoringDao;

    @Autowired
    private ProposalDao proposalDao;

    @Autowired
    private GrantCallModuleDao grantCallModuleDao;

    @Autowired
    private ProposalModuleDao proposalModuleDao;

    @Autowired
    private PrintService printService;

    @Autowired
    private PersonDao personDao;

    @Override
    public ResponseEntity<Object> saveExtReviewQuestionnaire(ExtReviewQuestionnaire extReviewQuestionnaire) {
        extReviewQuestionnaire.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        extReviewQuestionnaire.setUpdateUser(AuthenticatedUser.getLoginUserName());
        reviewServiceDao.saveQuestionnaire(extReviewQuestionnaire);
        return new ResponseEntity<>(extReviewQuestionnaire, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> deleteExtReviewQuestionnaire(Integer extReviewQuestionnaireId) {
        reviewServiceDao.deleteQuestionnaire(extReviewQuestionnaireId);
        return new ResponseEntity<>(null, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExtReviewQuestionnaire(Integer extReviewId) {
        List<ExtReviewQuestionnaire> questionnaireList = new ArrayList<>();
        reviewServiceDao.fetchExtQuestionnaires(extReviewId).forEach(ques -> {
            ExtReviewQuestionnaire questionnaire = new ExtReviewQuestionnaire();
            BeanUtils.copyProperties(ques, questionnaire, "externalReview");
            Object[] queObj = (Object[]) questionnaireDAO.getQuestionnaireById(ques.getQuestionnaireNumber());
            Map<String, Object> queHeaderMap = new HashMap<>();
            queHeaderMap.put("QUESTIONNAIRE_ID", queObj[0]);
            queHeaderMap.put("QUEST_GROUP_TYPE_CODE", queObj[1]);
            queHeaderMap.put("IS_FINAL", queObj[2]);
            queHeaderMap.put("QUESTIONNAIRE_NUMBER", queObj[3]);
            queHeaderMap.put("VERSION_NUMBER", queObj[4]);
            queHeaderMap.put("QUESTIONNAIRE", queObj[5]);
            queHeaderMap.put("DESCRIPTION", queObj[6]);
            questionnaire.setQuestionnaireDetail(queHeaderMap);
            questionnaireList.add(questionnaire);
        });
        return new ResponseEntity<>(questionnaireList, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExtReviewServiceType() {
        return new ResponseEntity<>(reviewServiceDao.fetchExtReviewServiceTypes(), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExtReviewStatus() {
        return new ResponseEntity<>(reviewServiceDao.fetchExtReviewStatus(), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> createExternalReview(ExternalReview externalReview) {
        externalReview.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        externalReview.setUpdateUser(AuthenticatedUser.getLoginUserName());
        externalReview.setReviewRequestorId(AuthenticatedUser.getLoginPersonId());
        externalReview.setRequestDate(commonDao.getCurrentTimestamp());
        externalReview.setExtReviewStatusCode(EXT_REVIEW_DEFAULT_STATUS_CODE);
        reviewServiceDao.saveExternalReview(externalReview);
        if (externalReview.getModuleItemCode().equals(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL)) {
            externalReview.setReviewModuleCode(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL);
            Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(externalReview.getModuleItemKey()));
            if (proposal != null && proposal.getGrantCallId() != null && Boolean.TRUE.equals(externalReview.getExtReviewServiceType().getIsScoringNeeded())) {
                List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(proposal.getGrantCallId());
                if (grantCallScoringCriterias != null && !grantCallScoringCriterias.isEmpty()) {
                    grantCallScoringCriterias.forEach(grantCallScoringCriteria -> {
                        ExtReviewScoringCriteria scoringCriteria = new ExtReviewScoringCriteria(externalReview.getExtReviewID(),
                                grantCallScoringCriteria.getScoringCriteriaTypeCode(), commonDao.getCurrentTimestamp(), AuthenticatedUser.getLoginUserName());
                        reviewServiceDao.saveExtReviewScoringCriteria(scoringCriteria);
                    });
                }
            }
        }
        addProposalTemplate(externalReview.getExtReviewID(),Integer.parseInt(externalReview.getModuleItemKey()));
        Person person = personDao.getPersonDetailById(externalReview.getReviewRequestorId());
        externalReview.setPerson(person);
        externalReview.setExtReviewStatus(reviewServiceDao.fetchExtReviewStatusByID(externalReview.getExtReviewStatusCode()));
        addToExtReviewHistory(externalReview.getExtReviewID(), Constants.EXT_REVIEW_ACTION_TYPE_CREATED, externalReview.getExtReviewStatusCode());
        return new ResponseEntity<>(externalReview, HttpStatus.OK);
    }

    private void addToExtReviewHistory(Integer extReviewId, Integer extReviewActionTypeCode, Integer extReviewStatusCode) {
		ExtReviewHistory extReviewHistory = new ExtReviewHistory();
		extReviewHistory.setExtReviewId(extReviewId);
		extReviewHistory.setExtReviewActionTypeCode(extReviewActionTypeCode);
		extReviewHistory.setExtReviewStatusCode(extReviewStatusCode);
		extReviewHistory.setUpdateUser(AuthenticatedUser.getLoginUserName());
		extReviewHistory.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		reviewServiceDao.saveExtReviewHistory(extReviewHistory);
	}

    private void addProposalTemplate(Integer extReviewId,Integer moduleItemKey) {
			byte[] bFile = printService.getTemplateData(Constants.PROPOSAL_LETTER_TEMPLATE_TYPE_CODE);
			byte[] mergedOutput = printService.setMergePlaceHoldersOfProposal(bFile, moduleItemKey,AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName()
					, null, null,null);
			if (mergedOutput != null) {
				ExtReviewAttachments extReviewAttachment = new ExtReviewAttachments();
				extReviewAttachment.setExtReviewID(extReviewId);
				extReviewAttachment.setAttachmentTypeCode(DEFAULT_ATTACHMENT_TYPE_CODE);
				extReviewAttachment.setDescription("Proposal Overview");
				extReviewAttachment.setFileName("ProposalOverview.pdf");
				extReviewAttachment.setIsAttachmentMandatory("Y");
				extReviewAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				extReviewAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
				ExtReviewAttachmentFile extReviewAttachmentFile = new ExtReviewAttachmentFile();
				extReviewAttachmentFile.setAttachment(mergedOutput);
				reviewServiceDao.saveExtReviewAttachmentFile(extReviewAttachmentFile);
				extReviewAttachment.setFileDataId(extReviewAttachmentFile.getFileDataId());
				reviewServiceDao.saveExtReviewAttachment(extReviewAttachment);
			}
	}
    
    @Override
    public ResponseEntity<Object> updateExternalReview(ExternalReview externalReview) {
        externalReview.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        externalReview.setUpdateUser(AuthenticatedUser.getLoginUserName());
        if(Boolean.FALSE.equals(externalReview.getExtReviewServiceType().getIsScoringNeeded())){
        	 reviewServiceDao.deleteExtReviewScoringCriteria(externalReview.getExtReviewID());
        } else {
			if (externalReview.getModuleItemCode().equals(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL) && 
		         (Boolean.TRUE.equals(externalReview.getExtReviewServiceType().getIsScoringNeeded()) && !externalReview.getIsTypeChange())) {
		            Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(externalReview.getModuleItemKey()));
		            if (proposal != null && proposal.getGrantCallId() != null) {
		                List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(proposal.getGrantCallId());
		                if (grantCallScoringCriterias != null && !grantCallScoringCriterias.isEmpty()) {
		                    grantCallScoringCriterias.forEach(grantCallScoringCriteria -> {
		                        ExtReviewScoringCriteria scoringCriteria = new ExtReviewScoringCriteria(externalReview.getExtReviewID(),
		                                grantCallScoringCriteria.getScoringCriteriaTypeCode(), commonDao.getCurrentTimestamp(), AuthenticatedUser.getLoginUserName());
		                        reviewServiceDao.saveExtReviewScoringCriteria(scoringCriteria);
		                    });
		                }
		            }
	        }
        }
        reviewServiceDao.updateExternalReview(externalReview);
        return new ResponseEntity<>(externalReview, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExternalReview(ExternalReview extReview, Integer moduleItemCode) {
        extReview.setModuleItemCode(moduleItemCode);
        List<ExternalReview> externalReviewDtoList = new ArrayList<>();
        reviewServiceDao.fetchExtReviewByDetails(extReview).forEach(externalReview -> {
            ExternalReview review = new ExternalReview();
            BeanUtils.copyProperties(externalReview, review, "person");
            Person person = new Person();
            BeanUtils.copyProperties(externalReview.getPerson(), person);
            review.setPerson(person);
            externalReviewDtoList.add(review);
        });
        return new ResponseEntity<>(externalReviewDtoList, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExtReviewScoringCriteria(Integer extReviewID) {
        return new ResponseEntity<>(reviewServiceDao.fetchExtReviewScoringCriteria(extReviewID), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getProposalGrantCallAttachments(Integer proposalID) {
        Proposal proposal = proposalDao.fetchProposalById(proposalID);
        Map<String, Object> attachments = new HashMap<>();
        List<GrantCallAttachment> grantCallAttachments = new ArrayList<>();
        if (proposal != null && proposal.getGrantCallId() != null) {
        	grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentWithLastVersion(proposal.getGrantCallId());
        }
        attachments.put("grantCallAttachments", grantCallAttachments);
        attachments.put("proposalAttachments", proposalModuleDao.fetchProposalAttachmentByProposalIdWithLastVersion(proposalID));
        return new ResponseEntity<>(attachments, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> saveExtReviewAttachments(List<ExtReviewAttachments> extReviewAttachments,
                                                           List<ExtReviewAttachments> grantCallProposalAttachments, MultipartFile[] files) {
        List<ExtReviewAttachments> attachmentsList = new ArrayList<>();
        try {
            if (files != null && files.length > 0 && extReviewAttachments != null) {
                for (int i = 0; i < files.length; i++) {
                    ExtReviewAttachments extReviewAttachment = extReviewAttachments.get(i);
                    ExtReviewAttachmentFile extReviewAttachmentFile = new ExtReviewAttachmentFile();
                    extReviewAttachmentFile.setAttachment(files[i].getBytes());
                    reviewServiceDao.saveExtReviewAttachmentFile(extReviewAttachmentFile);
                    extReviewAttachment.setFileDataId(extReviewAttachmentFile.getFileDataId());
                    extReviewAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
                    extReviewAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
                    extReviewAttachment.setIsAttachmentMandatory("N");
                    extReviewAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(extReviewAttachment.getUpdateUser()));
                    reviewServiceDao.saveExtReviewAttachment(extReviewAttachment);
                    attachmentsList.add(extReviewAttachment);
                }
            }
            if (grantCallProposalAttachments != null) {
                grantCallProposalAttachments = saveProposalGrantCallAttachments(grantCallProposalAttachments);
                attachmentsList.addAll(grantCallProposalAttachments);
            }
        } catch (IOException e) {
            throw new ApplicationException("Exception while saving External Review Attachments/Grant Call Proposal Attachments", e,
                    "saveExtReviewAttachments");
        }
        return new ResponseEntity<>(attachmentsList, HttpStatus.OK);
    }

    private List<ExtReviewAttachments> saveProposalGrantCallAttachments(List<ExtReviewAttachments> extReviewAttachments) {
        List<ExtReviewAttachments> attachmentsList = new ArrayList<>();
        extReviewAttachments.forEach(extReviewAttachment -> {
            FileData fileData = commonDao.getFileDataById(extReviewAttachment.getFileDataId());
            ExtReviewAttachmentFile extReviewAttachmentFile = new ExtReviewAttachmentFile();
            extReviewAttachmentFile.setAttachment(fileData.getAttachment());
            reviewServiceDao.saveExtReviewAttachmentFile(extReviewAttachmentFile);
            extReviewAttachment.setAttachmentTypeCode(PROPOSAL_GRATCALL_TYPE_CODE);
            extReviewAttachment.setFileDataId(extReviewAttachmentFile.getFileDataId());
            extReviewAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
            extReviewAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
            extReviewAttachment.setIsAttachmentMandatory("N");
            reviewServiceDao.saveExtReviewAttachment(extReviewAttachment);
            ExtReviewAttachmentType attachmentType = reviewServiceDao.fetchExtReviewAttachmentTypeByID(PROPOSAL_GRATCALL_TYPE_CODE);
            extReviewAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(extReviewAttachment.getUpdateUser()));
            extReviewAttachment.setExtReviewAttachmentType(attachmentType);
            attachmentsList.add(extReviewAttachment);
        });
        return attachmentsList;
    }

    @Override
    public ResponseEntity<Object> getExtReviewAttachments(Integer extReviewID) {
        List<ExtReviewAttachments> attachmentList = new ArrayList<>();
        reviewServiceDao.fetchExtReviewAttachments(extReviewID).forEach(attachment -> {
            ExtReviewAttachments extReviewAttachments = new ExtReviewAttachments();
            BeanUtils.copyProperties(attachment, extReviewAttachments, "attachmentFile", "externalReview");
            extReviewAttachments.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
            attachmentList.add(extReviewAttachments);
        });
        return new ResponseEntity<>(attachmentList, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getExtReviewAttachmentTypes() {
        return new ResponseEntity<>(reviewServiceDao.fetchExtReviewAttachmentTypes(), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> deleteExtReviewAttachment(Integer extReviewAttachmentId) {
        ExtReviewAttachments extReviewAttachments = reviewServiceDao.fetchExtReviewAttachmentById(extReviewAttachmentId);
        reviewServiceDao.deleteExtReviewAttachment(extReviewAttachmentId);
        reviewServiceDao.deleteExtReviewAttachmentFile(extReviewAttachments.getFileDataId());
        return ResponseEntity.ok().build();
    }

    @Override
    public ResponseEntity<Object> updateExtReviewAttachments(MultipartFile file, ExtReviewAttachments extReviewAttachment) {
        try {
            ExtReviewAttachments extReviewAttachments = reviewServiceDao.fetchExtReviewAttachmentById(extReviewAttachment.getExtReviewAttachmentId());
            reviewServiceDao.deleteExtReviewAttachmentFile(extReviewAttachments.getFileDataId());
            ExtReviewAttachmentFile extReviewAttachmentFile = new ExtReviewAttachmentFile();
            extReviewAttachmentFile.setAttachment(file.getBytes());
            reviewServiceDao.saveExtReviewAttachmentFile(extReviewAttachmentFile);
            extReviewAttachments.setFileDataId(extReviewAttachmentFile.getFileDataId());
            extReviewAttachments.setFileName(extReviewAttachment.getFileName());
            extReviewAttachments.setUpdateTimestamp(commonDao.getCurrentTimestamp());
            extReviewAttachments.setUpdateUser(AuthenticatedUser.getLoginUserName());
            reviewServiceDao.saveExtReviewAttachment(extReviewAttachments);
            extReviewAttachment.setUpdateTimestamp(extReviewAttachments.getUpdateTimestamp());
            extReviewAttachment.setUpdateUser(extReviewAttachments.getUpdateUser());
            extReviewAttachment.setFileDataId(extReviewAttachmentFile.getFileDataId());
        } catch (Exception e) {
            throw new ApplicationException("Exception while updating External Review Attachments", e, "updateExtReviewAttachments");
        }
        return new ResponseEntity<>(extReviewAttachment, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<byte[]> downloadExtReviewAttachments(Integer extReviewAttachmentId) {
        ResponseEntity<byte[]> attachmentData = null;
        try {
            ExtReviewAttachments extReviewAttachments = reviewServiceDao.fetchExtReviewAttachmentById(extReviewAttachmentId);
            attachmentData = printService.setAttachmentContent(extReviewAttachments.getFileName(), extReviewAttachments.getAttachmentFile().getAttachment());
        } catch (Exception e) {
            throw new ApplicationException("Exception while downloading External Review Attachment", e, "downloadExtReviewAttachments");
        }
        return attachmentData;
    }

    @Override
    public ResponseEntity<Object> sendForReview(Integer extReviewID) {
    	Integer preProposalExtReviewId = null;
        ExternalReview externalReview = reviewServiceDao.fetchExtReviewByID(extReviewID);
        if (externalReview.getExtReviewServiceTypeCode().equals(Constants.EXTERNAL_REVIEW_TYPE_CODE_MAIN_PROPOSAL)) {
        	Integer proposalId = reviewServiceDao.getPreProposalIdForProposal(Integer.parseInt(externalReview.getModuleItemKey()));
        	if (proposalId != null) {
        		ExternalReview preProposalExtReview = reviewServiceDao.getLatestActivePreProposalExternalReviewBasedOnProposalId(proposalId);
        		if (preProposalExtReview != null) {
        			preProposalExtReviewId = preProposalExtReview.getExtReviewID();
        			copyExtReviewReviewers(preProposalExtReview, externalReview);
        		}
        	}
        }
        reviewServiceDao.updateExternalReviewStatus(extReviewID, SEND_FOR_REVIEW_STATUS_CODE, commonDao.getCurrentTimestamp(), AuthenticatedUser.getLoginUserName(), preProposalExtReviewId);
        Map<String, Object> response = new HashMap<>();
        response.put("extReviewID", extReviewID);
        response.put("extReviewStatusCode", SEND_FOR_REVIEW_STATUS_CODE);
        response.put("extReviewStatus", reviewServiceDao.fetchExtReviewStatusByID(SEND_FOR_REVIEW_STATUS_CODE));
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

	private void copyExtReviewReviewers(ExternalReview preProposalExtReview, ExternalReview externalReview) {
		List<ExtReviewReviewer> extReviewReviewers = reviewServiceDao.getExtReviewReviewersByExternalReviewId(preProposalExtReview.getExtReviewID());
		if (extReviewReviewers != null && !extReviewReviewers.isEmpty()) {
			extReviewReviewers.forEach(extReviewReviewer -> {
				ExtReviewReviewer reviewer = new ExtReviewReviewer();
				BeanUtils.copyProperties(extReviewReviewer, reviewer);
				reviewer.setReviewReviewerId(null);
				if (reviewer.getExtReviewReviewersStatusCode() != null && reviewer.getExtReviewReviewersStatusCode().equals(Constants.EXT_REV_REVIEWER_STATUS_CODE_FINALIZED)) {
					reviewer.setExtReviewersStatusCode(Constants.EXT_REVIEWERS_STATUS_CODE_ACTIVE);
				} else if(reviewer.getExtReviewReviewersStatusCode() != null && reviewer.getExtReviewReviewersStatusCode().equals(Constants.EXT_REV_REVIEWER_STATUS_CODE_RESERVED)){
					reviewer.setExtReviewersStatusCode(Constants.EXT_REVIEWERS_STATUS_CODE_ONHOLD);
				} else {
					reviewer.setExtReviewersStatusCode(Constants.EXT_REVIEWERS_STATUS_CODE_INACTIVE);
				}
				reviewer.setExtReviewReviewersStatusCode(null);
				reviewer.setExtRevReviewerFlowStatusCode(Constants.EXT_REV_REVIEWER_FLOW_STATUS_CODE_PENDING);
				reviewer.setExtReviewId(externalReview.getExtReviewID());
				reviewer.setUpdateUser(AuthenticatedUser.getLoginUserName());
				reviewer.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				reviewServiceDao.saveOrUpdateExtReviewReviewer(reviewer);
			});
		}
	}
}
