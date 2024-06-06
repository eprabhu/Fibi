package com.polus.formbuilder.customdataelement.VO;

import java.util.List;

import com.polus.formbuilder.customdataelement.pojo.CustomData;

import lombok.Data;

@Data
public class CustomDataResponse {

	private Integer customDataElementId;

	private String columnName;

	private String defaultValue;

	private String dataType;

	private String isRequired;

	private List<Object> options;

	private List<CustomData> answers;

	private Integer moduleItemCode;

	private String moduleItemKey;

	private Integer dataLength;

	private String lookupWindow;

	private String lookupArgument;

	private String filterType;

	private Integer orderNumber;

	private Boolean isActive;

	private String customElementName;

	private String isMultiSelectLookup;;

}
