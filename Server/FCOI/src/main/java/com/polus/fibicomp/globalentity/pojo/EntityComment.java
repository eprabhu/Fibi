package com.polus.fibicomp.globalentity.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Table(name = "ENTITY_COMMENT")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntityComment implements Serializable {

	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "ENTITY_COMMENT_ID")
    private int entityCommentId;

    @Column(name = "ENTITY_ID")
	private int entityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_COMMENT_FK1"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private GlobalEntity entity;

	@Column(name = "COMMENT_TYPE_CODE")
    private String commentTypeCode;

    @ManyToOne(optional = true)
    @JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_COMMENT_FK2"), name = "COMMENT_TYPE_CODE", referencedColumnName = "COMMENT_TYPE_CODE", insertable = false, updatable = false)
    private EntityCommentType commentType;

    @Column(name = "IS_PRIVATE", length = 1)
    private String isPrivate;

    @Column(name = "COMMENT")
    private String comment;

    @Column(name = "PARENT_COMMENT_ID")
    private Integer parentCommentId;

}
