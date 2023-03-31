import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { slowSlideInOut} from '../../../../../fibi/src/app/common/utilities/animations';

@Component({
  selector: 'app-view-entity-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  animations: [slowSlideInOut]
})
export class ViewEntityDetailsComponent implements OnInit {

  @Output() hideEntityDetails: EventEmitter<any> = new EventEmitter<any>();

  constructor(private _router:Router) { }

  ngOnInit() {
  }

  backToList(){
    this.hideEntityDetails.emit(false);
  }

  modifyEntity() {
    this._router.navigate(['/coi/create-disclosure/entity-details'])
  }
}
