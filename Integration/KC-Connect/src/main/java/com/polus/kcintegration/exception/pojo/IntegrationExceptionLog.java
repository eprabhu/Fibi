package com.polus.kcintegration.exception.pojo;

import java.io.Serializable;
import java.time.LocalDateTime;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "INTEGRATION_EXCEPTION_LOG")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IntegrationExceptionLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "integration_exception_log_seq")
	@SequenceGenerator(name = "integration_exception_log_seq", sequenceName = "integration_exception_log_seq", allocationSize = 1)
	private Long id;

	@Column(name = "EXCEPTION_MESSAGE")
	private String exceptionMessage;

	@Column(name = "EXCEPTION_TYPE")
	private String exceptionType;

	@Column(name = "UPDATE_TIMESTAMP")
	private LocalDateTime timestamp;

}
