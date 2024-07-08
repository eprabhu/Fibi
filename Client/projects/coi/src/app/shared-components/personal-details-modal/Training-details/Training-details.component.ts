import { Component, OnInit, Input } from '@angular/core';
import { Subscription } from 'rxjs';
import { PersonDetailsModalService } from '../person-details-modal.service';
import { TrainingDashboardRequest } from '../person-details-modal.interface';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../../app-constants';

@Component({
    selector: 'app-Training-details',
    templateUrl: './Training-details.component.html',
    styleUrls: ['./Training-details.component.scss']
})
export class TrainingDetailsComponent implements OnInit {
    @Input() personId: string = '';
    $subscriptions: Subscription[] = [];
    traniningDetails: [];

    constructor(private _personDetailService: PersonDetailsModalService, private _commonservice: CommonService) { }

    ngOnInit() {
        this.loadTrainigDetails();
    }

    loadTrainigDetails(): void {
        const REQUEST_OBJECT = new TrainingDashboardRequest(this.personId);
        this.$subscriptions.push(this._personDetailService.loadPersonTrainingList(REQUEST_OBJECT).subscribe((res: any) => {
            this.traniningDetails = res.trainings;
        },
        error => this._commonservice.showToast(HTTP_ERROR_STATUS, 'Fetching training details failed. Please try again.')));
    }

    openInFibi(personTrainingId: number): void {
        const url = this._commonservice.fibiApplicationUrl + `#/fibi/training-maintenance/person-detail?personTrainingId=${personTrainingId}`;
        window.open(url);
    }

}
