package com.polus.formbuilder.dto;

import com.polus.appcorelib.customdataelement.vo.CustomDataElementVO;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FormSectionComponentBasicDTO {

	private String moduleSubItemKey;
		
	private Integer componentId;
	
	private String componentType;
	
	private ProgrammedElementModel programmedElement;
	
	private QuestionnaireDataBus questionnaire;
	
	private CustomDataElementVO customElement;
	
}