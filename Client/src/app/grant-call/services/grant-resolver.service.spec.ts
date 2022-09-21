import { TestBed, inject } from '@angular/core/testing';

import { GrantCallResolverService } from './grant-resolver.service';

describe('GrantResolverService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [GrantCallResolverService]
    });
  });

  it('should be created', inject([GrantCallResolverService], (service: GrantCallResolverService) => {
    expect(service).toBeTruthy();
  }));
});
