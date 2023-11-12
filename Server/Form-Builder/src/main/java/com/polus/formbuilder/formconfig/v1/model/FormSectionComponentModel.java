package com.polus.formbuilder.formconfig.v1.model;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FormSectionComponentModel {
		
		private int formBuilderSectCompId;
	    private int formBuilderSectionId;
	    private int formBuilderId;
	    private String componentTypeCode;
	    private Integer componentOrderNumber;
	    private String componentData;
	    private String componentRefId;
	    private String description;
	    private String headerInstruction;
	    private String footerInstruction;
	    private String isActive;
	    private Date updateTimestamp;
	    private String updateUser;
}
