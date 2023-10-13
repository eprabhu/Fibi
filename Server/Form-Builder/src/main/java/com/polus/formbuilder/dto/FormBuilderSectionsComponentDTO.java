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
public class FormBuilderSectionsComponentDTO {

	private Integer componentId;
	
	private Integer sectionId;
	
	private String componentDescription;
	
	private String componentType;
	
	private Integer ComponentOrder;
	
	private String componentRefId;
	
	private String componentData;
	
	private String componentHeader;
	
	private String componentFooter;
	
	private ProgrammedElementModel programmedElement;
	
	private QuestionnaireDataBus questionnaire;
	
	private CustomDataElementVO customElement;
	
}