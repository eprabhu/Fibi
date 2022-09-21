package com.polus.fibicomp.integratefile.dao;

import com.polus.fibicomp.integratefile.pojo.PersonFeed;

public interface IntegrationFileDao {

	/**
	 * This method is used to save or update person feed details.
	 * @return person feed details.
	 */
	public PersonFeed savePersonFeedDetails(PersonFeed PersonFeed);

	/**
	 * This method is used to delete person feed details.
	 * @return .
	 */
	public void deleteAllPersonFeed();

	/**
	 * This method is used to save or update person feed details to person table.
	 * @return boolean value.
	 */
	public boolean savePersonFeedDetailToPerson();

}
