import { Component, OnInit, Input } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { PersonRolodexViewService } from '../person-rolodex-view.service';
import { TrainingDashboardRequest } from '../../../admin-modules/person-training/person-training.interface';
import { HTTP_ERROR_STATUS } from '../../../app-constants';

@Component({
	selector: 'app-training-details',
	templateUrl: './training-details.component.html',
	styleUrls: ['./training-details.component.css']
})
export class TrainingDetailsComponent implements OnInit {
	@Input() trainingStatus;
	@Input() personRolodexId;
	personCertificationHistory = [];
	canViewTrainingDetails = false;
	$subscriptions: Subscription[] = [];
	isMaintainTraining = false;


	constructor(public __rolodexViewServices: PersonRolodexViewService, public _commonService: CommonService) { }

	ngOnInit() {
		this.getPermissions();
		this.loadTrainingDetails();
	}

	loadTrainingDetails() {
		this.canViewTrainingDetails = this.hasTrainingRightOrIsLoggedInUser(this.personRolodexId);
		this.$subscriptions.push(this.__rolodexViewServices.loadPersonTrainingList(
			new TrainingDashboardRequest(this.personRolodexId)).subscribe((data: any) => {
				this.personCertificationHistory = data.trainings;
			}, _err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching training details failed. Please try again.')));
	}
	async getPermissions() {
		this.isMaintainTraining = await this._commonService.checkPermissionAllowed('MAINTAIN_TRAINING');
	}

	hasTrainingRightOrIsLoggedInUser(id) {
		return this.isMaintainTraining ||
			(id === this._commonService.getCurrentUserDetail('personID'));
	}
}
