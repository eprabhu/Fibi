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
public class FormDashboardResponseModel {
	
	private int formBuilderId;
    private String formBuilderNumber;
    private int versionNumber;
    private String versionStatus;
    private String title;
    private String description;
    private String isActive;
    private String updateUser;
    private Date updateTimestamp;
    private String createUser;
}
