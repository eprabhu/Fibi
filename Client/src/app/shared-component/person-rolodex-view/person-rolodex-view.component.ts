import { Component, Input, EventEmitter, OnInit, Output } from '@angular/core';
import { forkJoin, Subscription } from 'rxjs';
import { Router } from '@angular/router';

import { PersonRolodexViewService } from './person-rolodex-view.service';
import { TrainingDashboardRequest } from '../../admin-modules/person-training/person-training.interface';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-person-rolodex-view',
    templateUrl: './person-rolodex-view.component.html',
    styleUrls: ['./person-rolodex-view.component.css'],
    providers: [PersonRolodexViewService]
})
export class PersonRolodexViewComponent implements OnInit {
    @Input() personRolodexType;
    @Input() personRolodexIsTraining;
    @Input() personRolodexPersonDescription;
    @Input() personRolodexId;
    @Input() personRolodexTrainingStatus;
    @Input() personRolodexIsDegree = false;
    @Input() personRolodexProposalPersonId = null;
    @Output() personRolodexViewModal: EventEmitter<any> = new EventEmitter<any>();
    @Input() personRolodexIsViewMode = false;

    $subscriptions: Subscription[] = [];
    selectedPersonDetails: any = {};
    personCertificationHistory = [];
    type: string;
    id: string;
    isTraining: false;
    personDescription: string;
    canViewTrainingDetails = false;
    isMaintainTraining = false;
    currentTab = 'PersonRolodexDetails';

    constructor(public __rolodexViewServices: PersonRolodexViewService, public _commonService: CommonService, private _router: Router) { }

    ngOnInit() {
        this.type = this.personRolodexType;
        this.id = this.personRolodexId;
        this.personDescription = this.personRolodexPersonDescription;
        this.isTraining = this.personRolodexIsTraining;
        this.fetchPersonRolodexDetails();
    }

    fetchPersonRolodexDetails(): void {
        if (this.type === 'PERSON') {
            this.$subscriptions.push(this.__rolodexViewServices.getPersonData(this.id).subscribe((data: any) => {
                this.updateSelectedPersonDetails(data.person);
                document.getElementById('app-view-non-employee-btn').click();
            }, _err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching person details failed. Please try again.')));
        } else {
            this.$subscriptions.push(this.__rolodexViewServices.getRolodexData(this.id).subscribe((data: any) => {
                this.updateSelectedPersonDetails(data.rolodex);
                this.selectedPersonDetails.organization = data.rolodex.organizations &&
                    data.rolodex.organizations.organizationName || data.rolodex.organizationName || null;
                document.getElementById('app-view-non-employee-btn').click();
            }, _err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching rolodex details failed. Please try again.')));
        }
    }

    updateSelectedPersonDetails(selectedPersonDetails) {
        this.selectedPersonDetails = selectedPersonDetails;
        this.selectedPersonDetails.proposalPersonRole = this.personRolodexPersonDescription;
        this.selectedPersonDetails.trainingStatus = this.isTraining ? this.personRolodexTrainingStatus : null;
    }

    emitPersonRolodexResult() {
        this.personRolodexViewModal.emit({ 'isPersonRolodexViewModal': false });
    }
}
