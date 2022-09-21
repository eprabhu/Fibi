package com.polus.fibicomp.triagequestionnaire.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.service.AgreementCopyService;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.triagequestionnaire.dao.TriageQuestionnaireDao;
import com.polus.fibicomp.triagequestionnaire.pojo.TriageHeader;
import com.polus.fibicomp.triagequestionnaire.vo.TriageQuestionnaireVo;

@Transactional
@Service(value = "triageQuestionnaireService")
public class TriageQuestionnaireServiceImpl implements TriageQuestionnaireService {

	protected static Logger logger = LogManager.getLogger(TriageQuestionnaireServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private TriageQuestionnaireDao triageQuestionnaireDao;

	@Autowired
	private AgreementCopyService agreementCopyService;

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private AgreementDao agreementDao;

	@Override
	public String createTriageHeader(TriageQuestionnaireVo vo) {
		TriageHeader triageHeader = vo.getTriageHeader();
		triageHeader.setCreateTimestamp(commonDao.getCurrentTimestamp());
		triageHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		triageHeader.setTriageStatus(Constants.TRIAGE_STATUS_PENDING);
		vo.setTriageHeader(triageQuestionnaireDao.saveOrUpdateTriageHeader(triageHeader));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String evaluateTriageQuestionnaire(TriageQuestionnaireVo vo) {
		try {
			Integer templateId = triageQuestionnaireDao.evaluateTriageQuestionnaire(vo);
			TriageHeader triageHeader = triageQuestionnaireDao.getTriageHeaderById(vo.getTriageHeaderId());
			triageHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			triageHeader.setUpdateUser(vo.getUpdateUser());
			if (templateId != 0) {
				Integer moduleItemId = triageQuestionnaireDao.getModuleItemKeyBasedOnParms(templateId, Constants.AGREEMENT_MODULE_CODE);
				if (moduleItemId != 0) {
					AgreementVO agreementVO = new AgreementVO();
					agreementVO.setAgreementRequestId(moduleItemId);
					agreementVO.setUpdateUser(vo.getUpdateUser());
					agreementVO = agreementCopyService.copyAgreement(agreementVO);
					AgreementHeader agreementHeader = agreementVO.getAgreementHeader();
					if (agreementHeader != null && agreementHeader.getUnitNumber() == null) {
						agreementService.setAgreementHomeUnit(vo.getPersonId(), agreementVO);
						agreementDao.saveOrUpdateAgreement(agreementVO.getAgreementHeader());
					}
					String moduleItemKey = agreementVO.getAgreementRequestId().toString();
					triageHeader.setTriageStatus(Constants.TRIAGE_STATUS_CREATED);
					triageHeader.setModuleItemKey(moduleItemKey);
					triageQuestionnaireDao.saveOrUpdateTriageHeader(triageHeader);
					vo.setModuleItemKey(moduleItemKey);
				} else {
					triageHeader.setTriageStatus(Constants.TRIAGE_STATUS_NA);
					triageQuestionnaireDao.saveOrUpdateTriageHeader(triageHeader);
				}
			} else {
				triageHeader.setTriageStatus(Constants.TRIAGE_STATUS_NA);
				triageQuestionnaireDao.saveOrUpdateTriageHeader(triageHeader);
			}
			return commonDao.convertObjectToJSON(vo);
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Error Occured");
		}
	}

}
