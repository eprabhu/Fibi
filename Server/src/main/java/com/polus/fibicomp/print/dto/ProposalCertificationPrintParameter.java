package com.polus.fibicomp.print.dto;

import java.util.List;

public class ProposalCertificationPrintParameter {

	private List<QuestionnairePrintParameter>  certificationQuestionaire;
	private String personName;

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public List<QuestionnairePrintParameter> getCertificationQuestionaire() {
		return certificationQuestionaire;
	}

	public void setCertificationQuestionaire(List<QuestionnairePrintParameter> certificationQuestionaire) {
		this.certificationQuestionaire = certificationQuestionaire;
	}
	
}
