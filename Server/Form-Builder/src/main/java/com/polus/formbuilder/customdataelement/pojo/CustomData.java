package com.polus.formbuilder.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity(name = "fbCompCustomElementAnswer")
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "FB_COMP_CUSTOM_ELEMENT_ANSWER")
public class CustomData implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CUSTOM_DATA_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer customDataId;

	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customDataElementsId;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_SUB_ITEM_CODE")
	private Integer moduleSubItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MODULE_SUB_ITEM_KEY")
	private String moduleSubItemKey;

	@Column(name = "VALUE")
	private String value;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public CustomData(Integer customDataId, String value, String description) {
		super();
		this.customDataId = customDataId;
		this.value = value;
		this.description = description;
	}

}
