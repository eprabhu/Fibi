package com.polus.fibicomp.triagequestionnaire.dao;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.triagequestionnaire.pojo.TriageHeader;
import com.polus.fibicomp.triagequestionnaire.vo.TriageQuestionnaireVo;

@Service
public interface TriageQuestionnaireDao {

	public TriageHeader saveOrUpdateTriageHeader(TriageHeader triageHeader);

	public Integer evaluateTriageQuestionnaire(TriageQuestionnaireVo vo);

	public Integer getModuleItemKeyBasedOnParms(Integer templateId, Integer agreementModuleCode);

	public TriageHeader getTriageHeaderById(Integer triageHeaderId);

}
