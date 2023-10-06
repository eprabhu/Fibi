package com.polus.fibicomp.opa.pojo;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "OPA_DISCLOSURE_STATUS_TYPE")
public class OPADisclosureStatusType implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "OPA_DISCLOSURE_STATUS_CODE")
	private String opaDisclosureStatusCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;


	
}
