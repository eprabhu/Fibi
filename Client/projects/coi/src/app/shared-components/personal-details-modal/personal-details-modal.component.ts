import { Component, Output, Input, OnInit, EventEmitter } from '@angular/core';
import { Person_details_modalService } from './person-details-modal.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-personal-details-modal',
    templateUrl: './personal-details-modal.component.html',
    styleUrls: ['./personal-details-modal.component.scss']
})
export class personalDetailsModalComponent implements OnInit {

    constructor(private _personservice:Person_details_modalService, private _commonservice:CommonService) { }

    @Input() userdetails: any;
    @Output() closeModalPersonDetails: EventEmitter<boolean> = new EventEmitter<boolean>();

    currentTab = 'PersonfullDetails';
    $subscriptions: Subscription[] = [];
    personValues: any;

    ngOnInit() {
        document.getElementById('persondetailsTrigger').click();
        this.persondetails(this.userdetails);
    }

    addnewvalue(value): void {
        this.closeModalPersonDetails.emit(value);
    }

    persondetails(userDetails): void {
        this.$subscriptions.push(this._personservice.getPersonData(userDetails.personId).subscribe((data: any) => {
            this.personValues = data;
        },
        error => this._commonservice.showToast(HTTP_ERROR_STATUS, 'Fetching training details failed. Please try again.')));
    }


}