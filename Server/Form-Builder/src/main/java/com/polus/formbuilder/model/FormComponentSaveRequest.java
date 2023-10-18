package com.polus.formbuilder.model;

import com.polus.appcorelib.customdataelement.vo.CustomDataElementVO;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;

import lombok.Data;

@Data
public class FormComponentSaveRequest{
	
	private Integer formBuilderId;
	
	private String documentOwnerPersonId;
	
	private String moduleItemCode;
	
	private String moduleSubItemCode;
	
	private String moduleItemKey;
	
	private String moduleSubItemKey;
	
	private Integer componentId;
	
	private String componentType;
	
	private String componentData;
	
	private String componentRefId;
	
	private ProgrammedElementModel programmedElement;
	
	private QuestionnaireDataBus questionnaire;
	
	private CustomDataElementVO customElement;
}
