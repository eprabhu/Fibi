package com.polus.fibicomp.globalentity.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.core.util.JpaCharBooleanConversion;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Table(name = "ENTITY_FILE_DATA")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntityFileData implements Serializable {

	private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "FILE_DATA_ID")
    private String fileDataId;

    @Column(name = "FILE_PATH")
    private String filePath;

    @Column(name = "ORIGINAL_FILE_NAME")
    private String originalFileName;

    @Column(name = "FILE_NAME")
    private String fileName;

    @Column(name = "IS_ARCHIVED")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Character isArchived;

    @Column(name = "FILE")
    private byte[] file;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATED_BY")
    private String updatedBy;

}
