package com.polus.fibicomp.persontraining.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingAttachment;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingComment;
import com.polus.fibicomp.persontraining.pojo.Training;
import com.polus.fibicomp.person.vo.PersonVO;

@Transactional
@Service
public interface PersonTrainingDao {

	/**
	 * @param personTraining
	 */
	public void saveOrUpdatePersonTraining(PersonTraining personTraining);

	/**
	 * @param vo
	 * @return List<PersonTraining>
	 */
	public PersonVO getTrainingDashboard(PersonVO vo);

	/**
	 * @param personTrainingComment
	 */
	public void saveOrUpdateTrainingComments(PersonTrainingComment personTrainingComment);

	/**
	 * @param trainingCommentId
	 */
	public void deleteTrainingComments(Integer trainingCommentId);

	/**
	 * @param trainingId
	 * @return PersonTraining
	 */
	public PersonTraining getPersonTraining(Integer trainingId);

	/**
	 * @param searchString
	 * @return training list
	 */
	public List<Training> loadTrainingList(String searchString);

	/**
	 * @param attachment
	 */
	public void saveOrUpdateTrainingAttachment(PersonTrainingAttachment attachment);

	/**
	 * @param attachmentId
	 * @return PersonTrainingAttachment
	 */
	public PersonTrainingAttachment loadPersonTrainingAttachment(int attachmentId);

	/**
	 * @param trainingAttachmentId
	 */
	public void deleteTrainingAttachment(Integer trainingAttachmentId);

	/**
	 * @param personTrainingId
	 */
	public void deletePersonTraining(Integer personTrainingId);
}
