function v=house(x)
	v = x;
	v(1) += sign(x(1))*norm(x);
	v /= norm(v);
end

%t = rand(10,1);
%v = house(t);
%q = eye(rows(v)) - 2*v*v';

%q*q'

