package com.polus.fibicomp.reviewcomments.service;

import static java.util.stream.Collectors.groupingBy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import com.polus.fibicomp.fcoiDisclosure.dao.FcoiDisclosureDao;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjectEntityRel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.coi.clients.FormBuilderClient;
import com.polus.fibicomp.coi.clients.model.BlankFormRequest;
import com.polus.fibicomp.coi.clients.model.BlankFormResponse;
import com.polus.fibicomp.coi.clients.model.FormBuilderSectionsComponentDTO;
import com.polus.fibicomp.coi.clients.model.FormBuilderSectionsDTO;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.questionnaire.dto.QuestionnaireDataBus;
import com.polus.core.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.service.COIFileAttachmentService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dao.OPAReviewDao;
import com.polus.fibicomp.opa.pojo.OPAReview;
import com.polus.fibicomp.reviewcomments.dao.ReviewCommentDao;
import com.polus.fibicomp.reviewcomments.dto.ModuleSectionDetailsDto;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import com.polus.core.security.AuthenticatedUser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;

@Service("reviewCommentService")
@Transactional
public class ReviewCommentServiceImpl implements ReviewCommentService {

    @Autowired
    private ReviewCommentDao reviewCommentDao;

    @Autowired
    private COIFileAttachmentService coiFileAttachmentService;

    @Autowired
    private ConflictOfInterestDao conflictOfInterestDao;

    @Autowired
    private OPADao opaDao;

    @Autowired
    private PersonDao personDao;

    @Autowired
    private QuestionnaireService questionnaireService;

    @Autowired
    private FormBuilderClient formBuilderClient;

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private OPAReviewDao opaReviewDao;

    @Autowired
    private FcoiDisclosureDao fcoiDisclosureDao;

    @Override
    public ResponseEntity<Object> saveOrUpdateReviewComment(MultipartFile[] files, DisclComment disclComment) {
        try {
            disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
            if (disclComment.getCommentTags() != null) {
                disclComment.getCommentTags().forEach(tag -> tag.setUpdateUser(AuthenticatedUser.getLoginUserFullName()));
            }
            reviewCommentDao.saveObject(disclComment);
            disclComment.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
            if (files != null) {
                COIFileRequestDto request = COIFileRequestDto.builder()
                        .componentReferenceId(Integer.valueOf(disclComment.getModuleItemKey()))
                        .componentReferenceNumber(disclComment.getModuleItemNumber())
                        .attaStatusCode(null)
                        .attaTypeCode(null)
                        .commentId(disclComment.getCommentId())
                        .componentTypeCode(disclComment.getComponentTypeCode())
                        .file(null)
                        .documentOwnerPersonId(null)
                        .description(null)
                        .build();
                addReviewAttachment(files, request);
                List<DisclAttachment> attachments = reviewCommentDao.loadDisclAttachmentByCommentId(disclComment.getCommentId());
                disclComment.setAttachments(attachments);
            }
            if (disclComment.getModuleCode() == Constants.OPA_MODULE_CODE) {
                opaDao.updateOPADisclosureUpDetails(Integer.valueOf(disclComment.getModuleItemKey()),
                        commonDao.getCurrentTimestamp());
            } else if (disclComment.getModuleCode() == Constants.COI_MODULE_CODE) {
                fcoiDisclosureDao.updateDisclosureUpdateDetails(Integer.valueOf(disclComment.getModuleItemKey()));
            }
            return new ResponseEntity<>(disclComment, HttpStatus.OK);
        } catch (Exception e) {
            throw new ApplicationException("Unable to save/update data", e, Constants.JAVA_ERROR);
        }
    }

    public void addReviewAttachment(MultipartFile[] files, COIFileRequestDto request) {
        for (int i = 0; i < files.length; i++) {
            request.setFile(files[i]);
            coiFileAttachmentService.saveFileAttachment(request);
        }
    }

    /**
     * This method is used get comments based on certain giving condition
     * and process the comments to single layer parent child hierarchy
     *
     * @param reviewCommentsDto
     * @return
     */
    @Override
//    @org.springframework.transaction.annotation.Transactional(propagation = Propagation.NOT_SUPPORTED)
    public ResponseEntity<Object> fetchReviewComments(ReviewCommentsDto reviewCommentsDto) {
        List<DisclComment> reviewComments = reviewCommentDao.fetchReviewComments(reviewCommentsDto);
        Map<Integer, List<DisclComment>> childComments = reviewComments.stream().filter(disclComment ->
                        disclComment.getParentCommentId() != null)
                .collect(groupingBy(DisclComment::getParentCommentId));
        reviewComments.removeIf(disclComment -> disclComment.getParentCommentId() != null);
        reviewComments = reviewComments.stream().map(disclComment -> {
            disclComment.setChildComments(childComments.get(disclComment.getCommentId()));
            return disclComment;
        }).collect(Collectors.toList());
        HashMap<String, String> userNames = new HashMap<>();
        HashMap<String, String> proposalTitles = new HashMap<>();
        HashMap<String, String> awardTitles = new HashMap<>();
        HashMap<String, String> personEntityNames = new HashMap<>();
        BlankFormResponse formResponse = null;
        Map<Integer, FormBuilderSectionsDTO> builderSectionsDTOMap = new HashMap<>();
        updateCommentObjects(reviewComments, userNames, proposalTitles, awardTitles, personEntityNames,
                reviewCommentsDto.getIsSectionDetailsNeeded(), formResponse, builderSectionsDTOMap);
        return new ResponseEntity<>(reviewComments, HttpStatus.OK);
    }

    void updateCommentObjects(List<DisclComment> reviewComments, HashMap<String, String> userNames,
                              HashMap<String, String> proposalTitles, HashMap<String, String> awardTitles,
                              HashMap<String, String> personEntityNames, Boolean isSectionDetailsNeeded,
                              BlankFormResponse formResponse, Map<Integer, FormBuilderSectionsDTO> builderSectionsDTOMap) {
        reviewComments.forEach(commentObj -> {
            if (!userNames.containsKey(commentObj.getUpdateUser())) {
                userNames.put(commentObj.getUpdateUser(), personDao.getUserFullNameByUserName(commentObj.getUpdateUser()));
            }
            commentObj.setUpdateUserFullName(userNames.get(commentObj.getUpdateUser()));
            List<DisclAttachment> attachments = reviewCommentDao.loadDisclAttachmentByCommentId(commentObj.getCommentId());
            if (attachments != null || !attachments.isEmpty()) {
                commentObj.setAttachments(attachments);
            }
            if (commentObj.getChildComments() != null) {
                updateCommentObjects(commentObj.getChildComments(), userNames, proposalTitles, awardTitles, personEntityNames,
                        isSectionDetailsNeeded, formResponse, builderSectionsDTOMap);
            }
            if (commentObj.getCommentTags() != null) {
                commentObj.getCommentTags().forEach(tagObj ->
                        tagObj.setTagPersonFullName(personDao.getPersonFullNameByPersonId(tagObj.getTagPersonId())));
            }if (isSectionDetailsNeeded != null && isSectionDetailsNeeded) {
                if (commentObj.getModuleCode() == Constants.COI_MODULE_CODE) {
                    getModuleSectionDetails(commentObj, proposalTitles, awardTitles, personEntityNames);
                } else if (commentObj.getModuleCode() == Constants.OPA_MODULE_CODE) {
                    getCommentsFormDetails(commentObj, formResponse, builderSectionsDTOMap);
                    if (commentObj.getComponentTypeCode().equals("11") && commentObj.getSubModuleItemKey() != null) {
                    	OPAReview opaReview = opaReviewDao.getOPAReview(Integer.parseInt(commentObj.getSubModuleItemKey()));
                        ModuleSectionDetailsDto sectionDetails = new ModuleSectionDetailsDto();
                        Map<String, String> otherDetails = new HashMap<>();
                        otherDetails.put("location", opaReview.getReviewLocationType().getDescription());
                        otherDetails.put("reviewerStatus", opaReview.getReviewStatusType().getDescription());
                        if (opaReview.getAssigneePersonId() != null) {
                            otherDetails.put("assigneeName", personDao.getPersonFullNameByPersonId(opaReview.getAssigneePersonId()));
                        }
                        sectionDetails.setOtherDetails(otherDetails);
                        commentObj.setModuleSectionDetails(sectionDetails);
                    }
                }
            }

        });
    }

    private void getModuleSectionDetails(DisclComment reviewComments, HashMap<String, String> proposalTitles, HashMap<String, String> awardTitles, HashMap<String, String> personEntityNames) {
        if (reviewComments.getComponentTypeCode().equals("5") && reviewComments.getParentCommentId() == null && reviewComments.getSubModuleItemKey() != null) {
            PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(Integer.valueOf(reviewComments.getSubModuleItemKey()));
            if (personEntity != null) {
                reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                        .sectionName(personEntity.getCoiEntity().getEntityName())
                        .sectionId(String.valueOf(personEntity.getPersonEntityId())).build());
            }
        }
        if (reviewComments.getComponentTypeCode().equals("4") && reviewComments.getParentCommentId() == null && reviewComments.getSubModuleItemKey() != null) {
            QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
            questionnaireDataBus.setQuestionnaireId(Integer.valueOf(reviewComments.getSubModuleItemKey()));
            QuestionnaireDataBus questData = questionnaireService.getQuestionnaireDetails(questionnaireDataBus);
            reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                    .sectionName(String.valueOf(questData.getHeader().get("QUESTIONNAIRE_NAME"))).build());
        }
        if (reviewComments.getComponentTypeCode().equals("6") && reviewComments.getParentCommentId() == null
                && reviewComments.getSubModuleItemKey() != null && reviewComments.getSubModuleItemNumber() != null) {
            if (reviewComments.getSubModuleItemNumber().equals(Constants.DEV_PROPOSAL_MODULE_CODE.toString())) {
                getProposalDetail(reviewComments, proposalTitles);
            } else if (reviewComments.getSubModuleItemNumber().equals(Constants.AWARD_MODULE_CODE.toString())) {
                getAwardDetail(reviewComments, awardTitles);
            }
        }
        if (reviewComments.getComponentTypeCode().equals("6") && reviewComments.getParentCommentId() == null &&
                reviewComments.getSubModuleItemKey() != null && reviewComments.getSubModuleItemNumber() == null) {
            CoiDisclProjectEntityRel relationDetail = fcoiDisclosureDao.getCoiDisclProjectEntityRelById(Integer.valueOf(reviewComments.getSubModuleItemKey()));
            if (relationDetail != null && relationDetail.getCoiDisclProject().getModuleCode() == Constants.DEV_PROPOSAL_MODULE_CODE) {
                getProposalDetail(reviewComments, proposalTitles, relationDetail);
            } else if (relationDetail != null && relationDetail.getCoiDisclProject().getModuleCode() == Constants.AWARD_MODULE_CODE) {
                getAwardDetail(reviewComments, awardTitles, relationDetail);
            }
            if (relationDetail != null && relationDetail.getPersonEntityId() != null) {
                getPersonEntityDetail(reviewComments, personEntityNames, relationDetail);
            }
        }

        if (reviewComments.getComponentTypeCode().equals("8") && reviewComments.getSubModuleItemKey() != null) {
        	CoiReview coiReview = conflictOfInterestDao.loadCoiReview(Integer.parseInt(reviewComments.getSubModuleItemKey()));
            ModuleSectionDetailsDto sectionDetails = new ModuleSectionDetailsDto();
            Map<String, String> otherDetails = new HashMap<>();
            otherDetails.put("location", coiReview.getReviewLocationType().getDescription());
            otherDetails.put("reviewerStatus", coiReview.getReviewerStatusType().getDescription());
            if (coiReview.getAssigneePersonId() != null) {
                otherDetails.put("assigneeName", personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId()));
            }
            sectionDetails.setOtherDetails(otherDetails);
            reviewComments.setModuleSectionDetails(sectionDetails);
        }
    }

    private void getPersonEntityDetail(DisclComment reviewComments, HashMap<String, String> personEntityNames, CoiDisclProjectEntityRel relationDetail) {
        if (!personEntityNames.containsKey(relationDetail.getPersonEntityId())) {
            PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(Integer.valueOf(relationDetail.getPersonEntityId()));
            personEntityNames.put(String.valueOf(relationDetail.getPersonEntityId()), personEntity.getCoiEntity().getEntityName());
        }
        if (reviewComments.getModuleSectionDetails() == null) {
            reviewComments.setModuleSectionDetails(new ModuleSectionDetailsDto());
        }
        reviewComments.getModuleSectionDetails().setSubsectionName(personEntityNames.get(String.valueOf(relationDetail.getPersonEntityId())));
        reviewComments.getModuleSectionDetails().setSectionId(String.valueOf(relationDetail.getPersonEntityId()));
    }

    private void getAwardDetail(DisclComment reviewComments, HashMap<String, String> awardTitles, CoiDisclProjectEntityRel relationDetail) {
        if (!awardTitles.containsKey(relationDetail.getCoiDisclProject().getModuleItemKey())) {
            List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, null,
            		null, relationDetail.getCoiDisclProject().getModuleItemKey());

            awardTitles.put(relationDetail.getCoiDisclProject().getModuleItemKey(), awardDetails.get(0).getTitle());
        }
        reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                .sectionName(awardTitles.get(relationDetail.getCoiDisclProject().getModuleItemKey()))
                .sectionId(relationDetail.getCoiDisclProject().getModuleItemKey()).build());
    }

    private void getProposalDetail(DisclComment reviewComments, HashMap<String, String> proposalTitles, CoiDisclProjectEntityRel relationDetail) {
        if (!proposalTitles.containsKey(relationDetail.getCoiDisclProject().getModuleItemKey())) {
            List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, null,
                    null, relationDetail.getCoiDisclProject().getModuleItemKey());
            proposalTitles.put(relationDetail.getCoiDisclProject().getModuleItemKey(), proposalDetails.get(0).getTitle());
        }
        reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                .sectionName(proposalTitles.get(relationDetail.getCoiDisclProject().getModuleItemKey()))
                .sectionId(relationDetail.getCoiDisclProject().getModuleItemKey()).build());
    }

    private void getAwardDetail(DisclComment reviewComments, HashMap<String, String> awardTitles) {
        if (!awardTitles.containsKey(reviewComments.getSubModuleItemKey())) {
            List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, null,
                    null, reviewComments.getSubModuleItemKey());
            awardTitles.put(String.valueOf(reviewComments.getSubModuleItemKey()), awardDetails.get(0).getTitle());
        }
        reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                .sectionName(awardTitles.get(String.valueOf(reviewComments.getSubModuleItemKey())))
                .sectionId(String.valueOf(reviewComments.getSubModuleItemKey())).build());
    }

    private void getProposalDetail(DisclComment reviewComments, HashMap<String, String> proposalTitles) {
        if (!proposalTitles.containsKey(reviewComments.getSubModuleItemKey())) {
            List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, null,
                    null, reviewComments.getSubModuleItemKey());
            proposalTitles.put(String.valueOf(reviewComments.getSubModuleItemKey()), proposalDetails.get(0).getTitle());
        }
        reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                .sectionName(proposalTitles.get(String.valueOf(reviewComments.getSubModuleItemKey())))
                .sectionId(String.valueOf(reviewComments.getSubModuleItemKey())).build());
    }

    void getCommentsFormDetails(DisclComment reviewComments, BlankFormResponse formResponse, Map<Integer, FormBuilderSectionsDTO> builderSectionsDTOMap) {
        try {
            if (formResponse == null && reviewComments.getFormBuilderId() != null) {
                formResponse = formBuilderClient.getBlankForm(BlankFormRequest.builder()
                        .moduleItemCode(String.valueOf(Constants.COI_MODULE_CODE))
                        .moduleItemKey(String.valueOf(reviewComments.getFormBuilderId()))
                        .moduleSubItemCode("0")
                        .moduleSubItemKey("0")
                        .formBuilderId(reviewComments.getFormBuilderId())
                        .build()).getBody();
            }
            if (reviewComments.getFormBuilderId() != null && reviewComments.getFormBuilderSectionId() == null) {
                reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                        .sectionName(formResponse.getForm().getFormName())
                        .build());
            } else if (reviewComments.getFormBuilderSectionId() != null && reviewComments.getFormBuilderComponentId() == null) {
                if (!builderSectionsDTOMap.containsKey(reviewComments.getFormBuilderSectionId())) {
                    FormBuilderSectionsDTO sectionDto = formResponse.getForm().getFormSections().stream().filter(section ->
                            section.getSectionId().equals(reviewComments.getFormBuilderSectionId())).collect(Collectors.toList()).get(0);
                    builderSectionsDTOMap.put(sectionDto.getSectionId(), sectionDto);
                }
                reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                        .sectionName(builderSectionsDTOMap.get(reviewComments.getFormBuilderSectionId()).getSectionName()).build());
            } else if (reviewComments.getFormBuilderComponentId() != null) {
                if (!builderSectionsDTOMap.containsKey(reviewComments.getFormBuilderSectionId())) {
                    FormBuilderSectionsDTO sectionDto = formResponse.getForm().getFormSections().stream().filter(section ->
                            section.getSectionId().equals(reviewComments.getFormBuilderSectionId())).collect(Collectors.toList()).get(0);
                    builderSectionsDTOMap.put(sectionDto.getSectionId(), sectionDto);
                }
                FormBuilderSectionsComponentDTO component = builderSectionsDTOMap.get(reviewComments.getFormBuilderSectionId()).getSectionComponent().stream().filter(formBuilderSectionsComponentDTO ->
                        formBuilderSectionsComponentDTO.getComponentId().equals(reviewComments.getFormBuilderComponentId())).collect(Collectors.toList()).get(0);
                reviewComments.setModuleSectionDetails(ModuleSectionDetailsDto.builder()
                        .subsectionName(component.getComponentHeader())
                        .sectionName(builderSectionsDTOMap.get(reviewComments.getFormBuilderSectionId()).getSectionName()).build());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    @Override
    public ResponseEntity<Object> deleteReviewComment(Integer reviewCommentId) {
        DisclComment disclComment = reviewCommentDao.fetchReviewCommentByCommentId(reviewCommentId);
        if (disclComment == null) {
            return new ResponseEntity<>("No data found", HttpStatus.NO_CONTENT);
        }
        List<Integer> childCommentIds = reviewCommentDao.getAllChildCommentId(reviewCommentId);
        childCommentIds = childCommentIds != null ? childCommentIds : new ArrayList<>();
        childCommentIds.add(reviewCommentId);
        childCommentIds.forEach(commentId ->
                reviewCommentDao.loadDisclAttachmentByCommentId(commentId).forEach(attachment -> {
                    COIFileRequestDto request = COIFileRequestDto.builder().attachmentId(attachment.getAttachmentId())
                            .fileDataId(attachment.getFileDataId()).build();
                    coiFileAttachmentService.deleteDisclAttachment(request);
                }));
        reviewCommentDao.deleteReviewComment(reviewCommentId);
        if (disclComment.getModuleCode() == Constants.OPA_MODULE_CODE) {
            opaDao.updateOPADisclosureUpDetails(Integer.valueOf(disclComment.getModuleItemKey()),
                    disclComment.getUpdateTimestamp());
        } else if (disclComment.getModuleCode() == Constants.COI_MODULE_CODE) {
            fcoiDisclosureDao.updateDisclosureUpdateDetails(Integer.valueOf(disclComment.getModuleItemKey()));
        }
        Map<String, Object> response = new HashMap<>();
        response.put("status", true);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> deleteReviewAttachment(Integer attachmentId) {
        Integer commentId = coiFileAttachmentService.deleteDisclAttachmentById(attachmentId);
        DisclComment disclComment = reviewCommentDao.fetchReviewCommentByCommentId(commentId);
        if (disclComment.getModuleCode() == Constants.OPA_MODULE_CODE) {
            opaDao.updateOPADisclosureUpDetails(Integer.valueOf(disclComment.getModuleItemKey()),
                    disclComment.getUpdateTimestamp());
        } else if (disclComment.getModuleCode() == Constants.COI_MODULE_CODE) {
            fcoiDisclosureDao.updateDisclosureUpdateDetails(Integer.valueOf(disclComment.getModuleItemKey()));
        }
        Map<String, Object> response = new HashMap<>();
        response.put("status", true);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<byte[]> downloadReviewAttachment(Integer attachmentId) {
        return coiFileAttachmentService.downloadDisclAttachment(attachmentId);
    }
}
