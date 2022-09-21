package com.polus.fibicomp.triagequestionnaire.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.triagequestionnaire.vo.TriageQuestionnaireVo;

@Service
public interface TriageQuestionnaireService {

	public String createTriageHeader(TriageQuestionnaireVo vo);

	public String evaluateTriageQuestionnaire(TriageQuestionnaireVo vo);
}
