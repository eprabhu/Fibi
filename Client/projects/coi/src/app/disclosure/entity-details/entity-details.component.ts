import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';

@Component({
  selector: 'app-entity-details',
  templateUrl: './entity-details.component.html',
  styleUrls: ['./entity-details.component.scss']
})
export class EntityDetailsComponent implements OnInit {

  constructor(private _entityDetails:EntityDetailsService,private _route:ActivatedRoute) { }
  entityDetails = {};
  ngOnInit() {

  }

}
