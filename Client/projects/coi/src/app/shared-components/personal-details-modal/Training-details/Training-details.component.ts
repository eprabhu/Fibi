import { Component, OnInit, Input } from '@angular/core';
import { Subscription } from 'rxjs';
import { Person_details_modalService } from '../person-details-modal.service';
import { TrainingDashboardRequest } from '../person-details-modal.interface';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../../app-constants';

@Component({
    selector: 'app-Training-details',
    templateUrl: './Training-details.component.html',
    styleUrls: ['./Training-details.component.scss']
})
export class trainingDetailsComponent implements OnInit {
    @Input() userdetails: any;
    $subscriptions: Subscription[] = [];
    traniningDetails: [];

    constructor(private _personDetailService: Person_details_modalService, private _commonservice: CommonService) { }

    ngOnInit() {
        this.loadTrainigDetails();
    }

    loadTrainigDetails():void {
        const requestObject = new TrainingDashboardRequest(this.userdetails.personId);
        this.$subscriptions.push(this._personDetailService.loadPersonTrainingList(requestObject).subscribe((res: any) => {
            this.traniningDetails = res.trainings;
        },
        error => this._commonservice.showToast(HTTP_ERROR_STATUS, 'Fetching training details failed. Please try again.')));
    }

    openInFibi(personTrainingId):void {
        const url = this._commonservice.fibiApplicationUrl + `#/fibi/training-maintenance/person-detail?personTrainingId=${personTrainingId}`
        window.open(url);
    }

}
