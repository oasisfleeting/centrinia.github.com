// matrix.js

function Matrix(rows,columns) {
	this.rows = rows;
	this.columns = columns;
	this.data = new Array(rows*columns);
};
Matrix.zero = function(rows,columns) {
	var c = new Matrix(rows,columns);
	for(var i=0;i<c.data.length;i++) {
		c.data[i] = 0;
	}
	return c;
};
Matrix.identity = function(dimension) {
	var c = Matrix.zero(dimension,dimension);
	for(var i=0;i<dimension;i++) {
		c.data[(dimension+1)*i] = 1;
	}
	return c;
};
Matrix.prototype.set_at = function(row,column,value) {
	if(row>=this.rows) {
		for(var i=this.rows;i<=row;i++) {
			for(var j=0;j<this.columns;j++) {
				this.data[i*this.columns+j] = 0;
			}
		}
		this.rows = row+1;
	}
	if(column>=this.columns) {
		for(var i=this.columns;i<=column;i++) {
			for(var j=0;j<this.rows;j++) {
				this.data[j*this.columns+i] = 0;
			}
		}
		this.columns = column+1;
	}
	this.data[row*this.columns+column] = value;
	return this;
}

Matrix.frustum = function (limits) {
	var result = Matrix.zero(limits.length+1,limits.length+1);
	var near = limits[limits.length-1][0];
	var far = limits[limits.length-1][1];
	for(var i=0;i<limits.length-1;i++) {
		var denominator = limits[i][1]-limits[i][0];
		result.set_at(i,i,2*near/denominator);
		result.set_at(i,limits.length-1,(limits[i][1]+limits[i][0])/denominator);
	}
	denominator = far-near;
	result.set_at(limits.length-1,limits.length-1,-(far+near)/denominator);
	result.set_at(limits.length-1,limits.length,-(2*far*near)/denominator);
	result.set_at(limits.length,limits.length-1,-1);
	return result;
};
Matrix.prototype.qr = function () {
	function householder(x) {
		var x_norm2_squared = 0;
		for(var i=1;i<x.length;i++) {
			x_norm2_squared += x[i]*x[i];
		}
		var x_norm = Math.sqrt(x[0]*x[0]+x_norm2_squared);
		var alpha = x[0] + (x[0] > 0 ? x_norm : -x_norm);

		var u_norm_squared = alpha*alpha+x_norm2_squared;

		return {'alpha': alpha, 'norm squared' : u_norm_squared};
	}

	var q = Matrix.identity(this.rows);
	var r = this.copy();
	for(var i=0;i<this.rows;i++) {
		q.data[i*(this.rows+1)] = 1;
	}

	for(var i=0;i<r.columns;i++) {
		var x = new Array(r.rows-i);
		for(var j=i;j<r.rows;j++) {
			x[j-i] = r.data[j*r.columns+i];
		}
		// v = [alpha,x(2:)] / sqrt(u_norm_squared)
		var reflector = householder(x);
		// Q_{k+1} = Q_k*(I-2*v*v')'; compute 2*Q_k*v/||v|| first
		var qv = new Array(q.rows);
		for(var j=0;j<qv.length;j++) {
			qv[j] = q.data[(j)*q.columns+(i)] * reflector['alpha'];
			for(var k=1;k<x.length;k++) {
				qv[j] += q.data[(j)*q.columns+(i+k)] * x[k];
			}
			qv[j] *= 2 / reflector['norm squared'];
		}

		// Update Q_{k+1} = Q_k - (2*Q_k*v)*v'
		for(var j=0;j<qv.length;j++) {
			q.data[(j)*q.columns+(i)] -= qv[j]*reflector['alpha'];
			for(var k=1;k<x.length;k++) {
				q.data[(j)*q.columns+(i+k)] -= qv[j]*x[k];
			}
		}

		// R_{k+1} = (I-2*v*v')*R_k; compute 2*v'*R_k/||v|| first.
		var rv = new Array(r.columns-i);
		for(var j=0;j<rv.length;j++) {
			rv[j] = r.data[(i)*r.columns+(i+j)] * reflector['alpha'];
			for(var k=1;k<x.length;k++) {
				rv[j] += r.data[(i+k)*r.columns+(i+j)] * x[k];
			}
			rv[j] *= 2 / reflector['norm squared'];
		}
		// Update R_{k+1} = R_k - v*(2*v'*R_k)
		for(var j=0;j<rv.length;j++) {
			r.data[(i)*r.columns+(i+j)] -= reflector['alpha']*rv[j];
			for(var k=1;k<x.length;k++) {
				r.data[(i+k)*r.columns+(i+j)] -= x[k]*rv[j];
			}
		}
	}
	return {'Q' : q, 'R' : r};
}

Matrix.prototype.transpose = function() {
	var c = new Matrix(this.rows,this.columns);
	for(var i=0;i<c.rows;i++) {
		for(var j=0;j<c.columns;j++) {
			c.data[i*c.columns+j] = this.data[j*this.columns+i];
		}
	}
	return c;
}
Matrix.prototype.copy = function() {
	var c = new Matrix(this.rows,this.columns);
	for(var i=0;i<this.data.length;i++) {
		c.data[i] = this.data[i];
	}
	return c;
}
Matrix.prototype.slice = function(row,column,rows,columns) {
	var c = new Matrix(rows,columns);
	for(var i=0;i<rows;i++) {
		for(var j=0;j<columns;j++) {
			c.data[i*columns+j] = this.data[(i+row)*this.columns+(j+column)];
		}
	}
	return c;
}
Matrix.prototype.scale = function(s) {
	var c = this.copy();
	for(var i=0;i<c.rows;i++) {
		for(var j=0;j<s.length;j++) {
			c.data[i*c.columns+j] *= s[j];
		}
	}
	return c;
}
Matrix.prototype.translate = function(b) {
	var c = this.copy();
	var v = this.multiply_vector(b.copy().set_at(b.dimension,1));
	for(var i=0;i<v.dimension;i++) {
		c.data[(i+1)*this.columns-1] = v.coord[i];
	}
	return c;
}
Matrix.prototype.rotate = function(angle,basis_vectors) {
	var basis = Matrix.zero(this.columns, basis_vectors.length);
	for(var i=0;i<basis_vectors.length;i++) {
		for(var j=0;j<basis_vectors[i].dimension;j++) {
			basis.data[j*basis.columns+i] = basis_vectors[i].coord[j];
		}
	}
	var cs = Math.cos(angle);
	var sn = Math.sin(angle);
	var qr = basis.qr();
	var givens = Matrix.identity(this.columns);
	givens.data[0] = cs;
	givens.data[1] = -sn;
	givens.data[this.columns+0] = sn;
	givens.data[this.columns+1] = cs;
	var c = this.multiply(qr['Q']);
	c = c.multiply(givens);
	return c.multiply(qr['Q'].transpose());
}
Matrix.prototype.multiply = function(b) {
	if(b instanceof Vector) {
		return this.multiply_vector(b);
	}
	var c = Matrix.zero(this.rows,b.columns);
	for(var i=0;i<b.columns;i++) {
		for(var j=0;j<this.rows;j++) {
			for(var k=0;k<this.columns;k++) {
				c.data[j*b.columns+i] += this.data[j*this.columns+k] * b.data[k*b.columns+i];
			}
		}
	}
	return c;
}
Matrix.prototype.multiply_vector = function(b) {
	var c = new Vector(this.rows);
	for(var i=0;i<this.rows;i++) {
		c.coord[i] = 0;
		for(var j=0;j<this.columns;j++) {
			c.coord[i] += this.data[i*this.columns+j] * b.coord[j];
		}
	}
	return c;
}
Matrix.prototype.back_substitution = function (b) {
	var c = b.copy();
	for(var i=this.rows-1;i>=0;i--) {
		var t = c.coord[i];
		for(var j=i+1;j<this.columns;j++) {
			t -= this.data[i*this.columns+j] * c.coord[j];
		}
		c.coord[i] = t/this.data[i*(this.columns+1)];
	}
	return c;
}
Matrix.prototype.inverse = function() {
	var qr = this.qr();
	var r = qr['R'];
	var r_inverse = new Matrix(this.rows,this.columns);
	for(var i=0;i<this.columns;i++) {
		/*var b = new Vector(this.rows);
		for(var j=0;j<this.rows;j++) {
			b.coord[j] = r.data[j*this.columns+i];
		}*/
		var b = Vector.zero(this.rows);
		b.coord[i] = 1;

		b = r.back_substitution(b);
		for(var j=0;j<this.rows;j++) {
			r_inverse.data[j*this.columns+i] = b.coord[j];
		}
	}
	return r_inverse.multiply(qr['Q'].transpose());
}
function Vector(dimension) {
	this.dimension = dimension;
	this.coord = new Array(dimension);
}
Vector.create = function(coord) {
	var result = new Vector(coord.length);
	for(var i=0;i<result.dimension;i++) {
		result.coord[i] = coord[i];
	}
	return result;
};
Vector.zero = function(dimension) {
	var c = new Vector(dimension);
	for(var i=0;i<dimension;i++) {
		c.coord[i] = 0;
	}
	return c;
}
Vector.copy = function(vector) {
	return Vector.create(this.coord);
};
Vector.prototype.set_at = function(index, component) {
	if(index>=this.dimension) {
		for(var i=this.dimension;i<index;i++) {
			this.coord[i] = 0;
		}
		this.dimension = index+1;
	}
	this.coord[index] = component;
	return this;
};
// TODO: Make this n-dimensional.
Vector.prototype.cross = function(b) {
	var c = new Vector(3);
	c.coord[0] = this.coord[1] * b.coord[2] - this.coord[2] * b.coord[1];
	c.coord[1] = - (this.coord[0] * b.coord[2] - this.coord[2] * b.coord[0]);
	c.coord[2] = (this.coord[0] * b.coord[1] - this.coord[1] * b.coord[0]);
	return c;
};
Vector.prototype.dot = function(b) {
	var c = 0;
	for(var i=0;i<this.dimension;i++) {
		c += this.coord[i] * b.coord[i];
	}
	return c;
};
Vector.prototype.distance = function(b) {
	var c = 0;
	for(var i=0;i<this.dimension;i++) {
		var t = this.coord[i] - b.coord[i];
		c += t*t;
	}
	return Math.sqrt(c);
};
Vector.prototype.scale = function (s) {
	var c = new Vector(this.dimension);
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i] * s;
	}
	return c;
};
Vector.prototype.norm = function() {
	var n = 0;
	for(var i=0;i<this.dimension;i++) {
		n += this.coord[i] * this.coord[i];
	}
	return Math.sqrt(n);
};
Vector.prototype.normalize = function() {
	var c = new Vector(this.dimension);
	var t = this.norm();
	if(t != 0) {
		t = 1/t;
	}
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i] * t;
	}
	return c;
};
Vector.prototype.add = function(b) {
	var c = new Vector(this.dimension);
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i] + b.coord[i];
	}
	return c;
};
Vector.prototype.subtract = function(b) {
	var c = new Vector(this.dimension);
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i] - b.coord[i];
	}
	return c;
};
Vector.prototype.mix = function(b, t) {
	var c = new Vector(this.dimension);
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i]*(1-t) + b.coord[i]*t;
	}
	return c;
};
Vector.prototype.angle_to = function (vec) {
	var d = vec.normalize().dot(this.normalize());
	// |d| may be greater than unity because of rounding errors.
	d = Math.abs(d) < 1 ? d : (d < 0 ? -1 : 1);
	return Math.acos(d);
};
Vector.prototype.rotate2d = function (angle) {
	var cs = Math.cos(angle);
	var sn = Math.sin(angle);
	var c = Vector.create(this.coord);
	c.coord[0] = cs * this.coord[0] - sn*this.coord[1];
	c.coord[1] = sn * this.coord[0] + cs*this.coord[1];
	for(var i=2;i<this.coord.length;i++) {
		c.coord[i] = this.coord[i];
	}
	return c;
};
Vector.prototype.copy = function() {
	var c = new Vector(this.dimension);
	for(var i=0;i<this.dimension;i++) {
		c.coord[i] = this.coord[i];
	}
	return c;
}

