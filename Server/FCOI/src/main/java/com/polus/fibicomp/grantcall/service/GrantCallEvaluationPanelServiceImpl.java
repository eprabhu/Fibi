package com.polus.fibicomp.grantcall.service;

import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.grantcall.dao.GrantCallEvaluationPanelDao;
import com.polus.fibicomp.grantcall.vo.GrantCallEvaluationPanelVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.workflow.pojo.WorkflowMap;
import com.polus.fibicomp.workflow.pojo.WorkflowMapDetail;

@Transactional
@Service(value = "grantCallEvaluationService")
public class GrantCallEvaluationPanelServiceImpl implements GrantCallEvaluationPanelService {

	protected static Logger logger = LogManager.getLogger(GrantCallEvaluationPanelServiceImpl.class.getName());

	@Autowired
	private GrantCallEvaluationPanelDao grantCallEvaluationPanelDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	public CommonDao commonDao;

	@Override
	public String fetchAllEvaluationPanels(GrantCallEvaluationPanelVO vo) {
		if (commonDao.getParameterValueAsString(Constants.SIMPLE_EVALUATION_MAP_ID) != null) {
			vo.setWorkflowMaps(grantCallEvaluationPanelDao.fetchAllEvaluationPanels(Constants.SIMPLE_EVALUATION_PANEL_MAP_ID, vo.getMapType()));
		} else {
			vo.setWorkflowMaps(grantCallEvaluationPanelDao.fetchAllEvaluationPanels(null, vo.getMapType()));
		}
		List<WorkflowMap> workflowMapList = vo.getWorkflowMaps();
		if (workflowMapList != null && !workflowMapList.isEmpty()) {
			for (WorkflowMap workflowMap : workflowMapList) {
				List<WorkflowMapDetail> workflowMapDetailList = workflowMap.getWorkflowMapDetails();
				for (WorkflowMapDetail workflowMapDetail : workflowMapDetailList) {
					if (workflowMapDetail.getApproverPersonId() != null && !workflowMapDetail.getApproverPersonId().isEmpty()) {
						workflowMapDetail.setFullName(personDao.getPersonFullNameByPersonId(workflowMapDetail.getApproverPersonId()));
					}
				}
			}
		}
		vo.setWorkflowMaps(workflowMapList);
		vo.setGrantCallEvaluationPanels(grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(vo.getGrantCallId()));
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateGrantCallEvaluationPanel(GrantCallEvaluationPanelVO vo) {
		List<GrantCallEvaluationPanel> grantCallEvaluationPanelList = vo.getGrantCallEvaluationPanels();
		if (grantCallEvaluationPanelList != null && !grantCallEvaluationPanelList.isEmpty()) {
			for (GrantCallEvaluationPanel grantCallEvaluationPanel : grantCallEvaluationPanelList) {
				if (grantCallEvaluationPanel.getIsMainPanel() == null) {
					grantCallEvaluationPanel.setIsMainPanel(Constants.MAIN_PANEL_FOR_SIMPLE_EVALUATION_PANEL);
				}
				grantCallEvaluationPanelDao.saveOrUpdateGrantCallEvaluationPanel(grantCallEvaluationPanel);
			}
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteEvaluationPanel(GrantCallEvaluationPanelVO vo) {
		if (vo.getGrantCallEvaluationPanelId() != null) {
			String successMessage = grantCallEvaluationPanelDao.deleteGrantCallEvaluationPanel(vo.getGrantCallEvaluationPanelId());
			vo.setMessage(successMessage);
		}
		return committeeDao.convertObjectToJSON(vo);
	}


}
