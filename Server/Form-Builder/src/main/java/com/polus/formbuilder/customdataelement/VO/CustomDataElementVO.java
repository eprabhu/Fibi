package com.polus.formbuilder.customdataelement.VO;

import java.sql.Timestamp;
import java.util.List;

import com.polus.appcorelib.vo.LookUp;
import com.polus.formbuilder.customdataelement.pojo.CustomData;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElementOption;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElements;

import lombok.Data;

@Data
public class CustomDataElementVO {

	private CustomDataElements customDataElement;

	private List<CustomDataElements> customDataElements;

	private String responseMessage;

	private Integer customDataElementId;

	private List<CustomDataElementOption> elementOptions;

	private List<CustomData> customResponses;

	private Integer moduleCode;

	private Integer subModuleCode;

	private List<CustomDataResponse> customElements;

	private String updateUser;

	private Timestamp updateTimestamp;

	private String moduleItemKey;

	private String moduleSubItemKey;

	private List<CustomDataElementOption> deleteOptions;

	private String dataTypeCode;

	private List<LookUp> lookUps;

}
