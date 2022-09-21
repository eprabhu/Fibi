package com.polus.fibicomp.workflow.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowDetailExt;
import com.polus.fibicomp.workflow.vo.WorkflowVO;

@Transactional
@Service(value = "workflowService")
public class WorkflowServiceImpl implements WorkflowService {

	protected static Logger logger = LogManager.getLogger(WorkflowServiceImpl.class.getName());

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private PrintService printService;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private PersonDao personDao;

	public WorkflowDetail addWorkflowAttachments(String personId, String approverComment, MultipartFile[] files, WorkflowDetail workflowDetail) throws IOException {
		List<WorkflowAttachment> workflowAttachments = new ArrayList<WorkflowAttachment>();
		for (int i = 0; i < files.length; i++) {
			WorkflowAttachment workflowAttachment = new WorkflowAttachment();
			workflowAttachment.setDescription(approverComment);
			workflowAttachment.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			workflowAttachment.setUpdateUser(personId);			
			FileData fileData = new FileData();
			fileData.setAttachment(files[i].getBytes());
			fileData = commonDao.saveFileData(fileData);
			workflowAttachment.setFileDataId(fileData.getFileDataId());
			workflowAttachment.setFileName(files[i].getOriginalFilename());
			workflowAttachment.setMimeType(files[i].getContentType());
			workflowAttachment.setWorkflowDetail(workflowDetail);
			workflowAttachments.add(workflowAttachment);
		}
		workflowDetail.getWorkflowAttachments().addAll(workflowAttachments);
		return workflowDetail;	
	}

	@Override
	public ResponseEntity<byte[]> downloadWorkflowAttachment(Integer attachmentId) {
		WorkflowAttachment attachment = workflowDao.fetchWorkflowAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), attachment.getAttachment());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return attachmentData;
	}

	@Override
	public WorkflowDetail getCurrentWorkflowDetail(Integer workflowId, String personId, Integer roleCode) {
		return workflowDao.getCurrentWorkflowDetail(workflowId, personId, roleCode);
	}

	@Override
	public Set<String> getEmailAdressByUserType(String roleTypeCode) {
		return workflowDao.fetchEmailAdressByUserType(roleTypeCode);
	}

	@Override
	public void prepareWorkflowDetails(Workflow workflow) {
		Map<Integer, List<WorkflowDetail>> workflowDetailMap = new HashMap<Integer, List<WorkflowDetail>>();
		List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
		if (workflowDetails != null && !workflowDetails.isEmpty()) {
			Collections.sort(workflowDetails, new WorkflowDetailComparator());
			for (WorkflowDetail workflowDetail : workflowDetails) {

				if (workflowDetail.getApprovalStatusCode().equals("W")) {
					workflow.setCurrentStopName(workflowDao.fetchStopNameBasedMapIdAndStop(workflowDetail.getMapId(), workflowDetail.getApprovalStopNumber()));
				}
				if (workflowDetail.getUpdateUser() != null) {
					workflowDetail.setUpdateUserFullName(personDao.getUserFullNameByUserName(workflowDetail.getUpdateUser()));
				}
				if (workflow.getMapType() != null && workflow.getMapType().equals("E")) {
					if (proposalDao.checkForProposalEvaluationPanelRank(workflowDetail.getMapId(), Integer.parseInt(workflow.getModuleItemId()))) {
						workflowDetail.setIsReviewerCanScore(true);
					}
				}
				WorkflowDetailExt workflowDetailExt = workflowDao.fetchWorkflowExtBasedOnWorkflowDetailId(workflowDetail.getWorkflowDetailId());
				if (workflowDetailExt != null) {
					workflowDetail.setWorkflowDetailExt(workflowDetailExt);
				}
				if (workflowDetailMap.get(workflowDetail.getApprovalStopNumber()) != null) {
					workflowDetailMap.get(workflowDetail.getApprovalStopNumber()).add(workflowDetail);
				} else {
					List<WorkflowDetail> details = new ArrayList<>();
					details.add(workflowDetail);
					workflowDetailMap.put(workflowDetail.getApprovalStopNumber(), details);
				}
			}
		}
			workflow.setWorkflowDetailMap(workflowDetailMap);
	}

	@Override
	public void prepareWorkflowDetailsList(List<Workflow> workflowList) {
		for (Workflow workflow : workflowList) {
			Map<Integer, List<WorkflowDetail>> workflowDetailMap = new HashMap<Integer, List<WorkflowDetail>>();
			workflow.setWorkflowCreatedBy(personDao.getPersonFullNameByPersonId(workflow.getWorkflowStartPerson()));
			prepareWorkflowDetails(workflow);
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getUpdateUser() != null) {
						workflowDetail.setUpdateUserFullName(personDao.getUserFullNameByUserName(workflowDetail.getUpdateUser()));
					}
					WorkflowDetailExt workflowDetailExt = workflowDao.fetchWorkflowExtBasedOnWorkflowDetailId(workflowDetail.getWorkflowDetailId());
					if (workflowDetailExt != null) {
						workflowDetail.setWorkflowDetailExt(workflowDetailExt);
					}
					if (workflowDetailMap.get(workflowDetail.getApprovalStopNumber()) != null) {
						workflowDetailMap.get(workflowDetail.getApprovalStopNumber()).add(workflowDetail);
					} else {
						List<WorkflowDetail> details = new ArrayList<>();
						details.add(workflowDetail);
						workflowDetailMap.put(workflowDetail.getApprovalStopNumber(), details);
					}
					if (workflowDetail.getDelegatedByPersonId() != null) {
						workflowDetail.setDelegatedPersonName(personDao.getPersonFullNameByPersonId(workflowDetail.getDelegatedByPersonId()));
					}
				}
			}
			workflow.setWorkflowDetailMap(workflowDetailMap);
		}
	}

	@Override
	public String fetchWorkflowMapType() {
		WorkflowVO vo=new WorkflowVO();
        vo.setWorkflowMapType(workflowDao.fetchAllWorkflowMapTypes());
		return committeeDao.convertObjectToJSON(vo);
	}

}
